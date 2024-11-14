package rocket

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.{HellaCacheIO, M_XRD, M_XWR, TLB}

class Sweeper(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val blockAddr = Input(UInt(64.W))      // 64-bit virtual address of the block to sweep
    val blockPtrReady = Input(Bool())      // Valid signal from wrapper indicating a block is ready
    val sweeperReady = Output(Bool())      // Indicates completion of the sweeping process
    val mem = new HellaCacheIO             // TileLink memory interface
  })

  // Define FSM states
  val idle :: translateAddr :: loadMetadata :: loadFreeListHead :: loadCellHeader1 :: loadCellHeader2 :: updateFreeList :: writeBackHeaders :: done :: Nil = Enum(9)
  val state = RegInit(idle)

  // Registers for internal use
  val blockPAddr = Reg(UInt(64.W))         // Physical address of the block
  val cellSize = Reg(UInt(32.W))           // Size of each cell
  val numCells = Reg(UInt(32.W))           // Number of cells in the block
  val freeListHead = Reg(UInt(64.W))       // Free list head
  val currentCell = Reg(UInt(64.W))        // Current cell address being processed
  val currentHeader1 = Reg(UInt(64.W))     // First 64-bit header word of a cell
  val currentHeader2 = Reg(UInt(64.W))     // Second 64-bit header word of a cell
  val cellCount = Reg(UInt(32.W))          // Counter to track processed cells
  val markBitMask = 1.U                    // Mark bit mask
  val tagBitMask = 2.U                     // Tag bit mask
  val flBitMask = 4.U                      // Free list bit (FL) mask
  val newHeader1 = Wire(UInt(64.W))
  val newHeader2 = Wire(UInt(64.W))

  // Default outputs
  io.sweeperReady := false.B

  // Memory and TLB settings for TileLink
  io.mem.req.valid := false.B
  io.mem.req.bits.cmd := M_XRD // Default read command
  io.mem.req.bits.size := log2Ceil(8).U // 64-bit accesses

  // Initialize TLB
  val tlb = Module(new TLB(instruction = false, lgMaxSize = log2Ceil(8), nEntries = 32))
  tlb.io.req.valid := false.B
  tlb.io.req.bits.vaddr := io.blockAddr
  tlb.io.req.bits.size := log2Ceil(8).U
  tlb.io.req.bits.cmd := M_XRD

  switch(state) {
    is(idle) {
      io.sweeperReady := true.B
      when(io.blockPtrReady) {
        state := translateAddr
      }
    }

    is(translateAddr) {
      io.sweeperReady := false.B
      tlb.io.req.valid := true.B
      tlb.io.req.bits.vaddr := io.blockAddr
    when(tlb.io.req.ready){
      when(tlb.io.resp.valid) {
        blockPAddr := tlb.io.resp.bits.paddr
        state := loadMetadata
      }
    }
    }

    is(loadMetadata) {
      // Load metadata: 64-bit word at the block's physical address
      io.mem.req.valid := true.B
      io.mem.req.bits.addr := blockPAddr
      io.mem.req.bits.cmd := M_XRD
      when(io.mem.req.ready) {       
        when(io.mem.resp.valid) {
            io.mem.req.valid := false.B
            val metadata = io.mem.resp.bits.data
            cellSize := metadata(63, 32) // Extract cell size
            numCells := metadata(31, 0)  // Extract number of cells
            state := loadFreeListHead
        }
      }
    }

    is(loadFreeListHead) {
      // Load the free list head (next 64 bits)
      io.mem.req.valid := true.B
      io.mem.req.bits.addr := blockPAddr + 8.U
      io.mem.req.bits.cmd := M_XRD
      when(io.mem.req.ready) {       
        when(io.mem.resp.valid) {
            io.mem.req.valid := false.B
            freeListHead := io.mem.resp.bits.data
            cellCount := 0.U
            currentCell := blockPAddr + 16.U // Start processing cells at 128-bit offset
            state := loadCellHeader1
        }
      }
    }

    is(loadCellHeader1) {
      // Load the first 64 bits of the current cell's header
      io.mem.req.valid := true.B
      io.mem.req.bits.addr := currentCell
      io.mem.req.bits.cmd := M_XRD
      when(io.mem.req.ready) {       
        when(io.mem.resp.valid) {
           io.mem.req.valid := false.B
            currentHeader1 := io.mem.resp.bits.data
            state := loadCellHeader2
        }
      }
    }

    is(loadCellHeader2) {
      // Load the second 64 bits of the current cell's header
      io.mem.req.valid := true.B
      io.mem.req.bits.addr := currentCell + 8.U
      io.mem.req.bits.cmd := M_XRD
      when(io.mem.req.ready) {       
        when(io.mem.resp.valid) {
            io.mem.req.valid := false.B
            currentHeader2 := io.mem.resp.bits.data
            state := updateFreeList
        }
      }
    }

    is(updateFreeList) {
      // Examine mark, tag, and FL bits and update headers as needed
      val markBit = currentHeader1 & markBitMask
      val tagBit = (currentHeader1 & tagBitMask) >> 1
      val flBit = (currentHeader1 & flBitMask) >> 2

      // Clear the mark bit
      newHeader1 := currentHeader1 & ~markBitMask

      // If mark is 0, tag is 1, FL is 0, then update the FL bit and header2's FLNext pointer
      when(markBit === 0.U && tagBit === 1.U && flBit === 0.U) {
        newHeader1 := newHeader1 | flBitMask
        newHeader2 := Cat(freeListHead(31, 0), currentHeader2(63, 32)) // Set FLNext to previous freeListHead
        freeListHead := currentCell // Update free list head
      }.otherwise {
        newHeader2 := currentHeader2
      }
      state := writeBackHeaders
    }

    is(writeBackHeaders) {
      // Write updated headers back to memory
      io.mem.req.valid := true.B
      io.mem.req.bits.addr := currentCell
      io.mem.req.bits.cmd := M_XWR
      io.mem.req.bits.data := newHeader1
      when(io.mem.req.ready) {       
        when(io.mem.resp.valid) {
            // Write second header word
            io.mem.req.valid := true.B
            io.mem.req.bits.addr := currentCell + 8.U
            io.mem.req.bits.data := newHeader2
            when(io.mem.req.ready) {  
              when(io.mem.resp.valid) {
                io.mem.req.valid := false.B
                // Check if all cells processed
                cellCount := cellCount + 1.U
                when(cellCount === numCells) {
                    state := done
                }.otherwise {
                    currentCell := currentCell + cellSize
                    state := loadCellHeader1
                }
              }
            }
        }
      }
    }

    is(done) {
      io.sweeperReady := true.B
      state := idle
    }
  }
}
