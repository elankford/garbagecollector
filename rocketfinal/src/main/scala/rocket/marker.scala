package rocket

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.{HellaCacheIO, M_XRD, TLB}

class Marker(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val markQueueVAddrIn = Input(UInt(64.W))  // Input virtual address from mark queue
    val markQueueValidIn = Input(Bool())      // Valid signal indicating data in mark queue
    val outboundRefAddrOut = Output(UInt(64.W)) // Output reference physical address for tracing
    val outputDoneProcessing = Output(Bool())   // Output indicating the marker is idle
    val outputRefReady = Output(Bool())    // Output signaling that a reference address is ready
    val mem = new HellaCacheIO                 // TileLink memory interface for memory access
  })

  // Define state machine states for handling memory operations and address translations
  val idle :: translateAddr :: loadStatusWord :: setMarkBit :: loadNumRefs :: processRefs :: outputRef :: waitRefOffset :: loadRefOffset :: Nil = Enum(9)
  val state = RegInit(idle)

  // Internal Registers to store intermediate data
  val objVAddr = Reg(UInt(64.W))             // Holds the virtual address of the current object
  val objPAddr = Reg(UInt(64.W))             // Physical address of the current object (after translation)
  val statusWord = Reg(UInt(64.W))           // Stores the status word loaded from memory
  val tibVAddr = Reg(UInt(64.W))             // Virtual address for TIB 
  val numRefs = Reg(UInt(64.W))              // Number of outbound references from this object
  val refCounter = Reg(UInt(32.W))           // Counter for tracking processed references
  val refOffsetsAddr = Reg(UInt(64.W))       // Address of reference offsets within TIB
  val refAddr = Reg(UInt(64.W))              // Computed address of each reference
  val doneWithAddress = RegInit(false.B)

  // Default output values
  io.outboundRefAddrOut := refAddr          // Outbound reference address for other modules
  io.outputDoneProcessing := doneWithAddress     // Indicates when processing is completed

  
  io.outputRefReady := false.B              // Signal that outbound reference address is ready


  // Memory control signals, initially set for reads
  io.mem.req.valid := false.B               // Memory request is inactive by default
  io.mem.req.bits.cmd := M_XRD              // Default command is read; adjusted as necessary
  io.mem.req.bits.size := log2Ceil(8).U     // Size of access (8 bytes for 64-bit)

  // Instantiate TLB module for address translation
  val tlb = Module(new TLB(instruction = false, lgMaxSize = log2Ceil(8)))
  tlb.io.req.valid := false.B               // TLB request signal inactive by default
  tlb.io.req.bits.size := log2Ceil(8).U     // Access size (64 bits)
  tlb.io.req.bits.cmd := M_XRD              // Read command for TLB

  // FSM to control the marker's actions based on state
  switch(state) {
    is(idle) {
      // IDLE state: Wait for a valid input from the mark queue
      doneWithAddress := true.B       // Indicate idle state
      when(io.markQueueValidIn) {             // Check if mark queue has valid data
        objVAddr := io.markQueueVAddrIn       // Store input virtual address
        state := translateAddr                // Move to address translation state
        doneWithAddress := false.B    // Clear idle flag
      }
    }

    is(translateAddr) {
      // TRANSLATE_ADDR state: Translate object virtual address to physical address
      tlb.io.req.valid := true.B              // Send request to TLB
      tlb.io.req.bits.vaddr := objVAddr       // Set virtual address to be translated
      when(tlb.io.req.ready) {                // Wait until TLB is ready to accept request
        when(tlb.io.resp.valid) {             // Wait for valid TLB response
        tlb.io.req.valid := false.B
          objPAddr := tlb.io.resp.bits.paddr  // Save translated physical address
          state := loadStatusWord             // Move to next state to load status word
        }
      }
    }

    is(loadStatusWord) {
      // LOAD_STATUS_WORD state: Load status word from the physical address
      io.mem.req.valid := true.B              // Set memory request as valid
      io.mem.req.bits.addr := objPAddr        // Use object physical address
      io.mem.req.bits.cmd := M_XRD            // Read command
      when(io.mem.req.ready) {                // Wait until memory is ready to accept request
        when(io.mem.resp.valid) {             // Wait for memory response
        io.mem.req.valid := false.B
          statusWord := io.mem.resp.bits.data // Store retrieved status word
          state := setMarkBit                 // Move to set mark bit state
        }
      }
    }

    is(setMarkBit) {
      // SET_MARK_BIT state: Set the mark bit in the status word and write back
      statusWord := statusWord | 1.U          // Set the mark bit (LSB) to 1
      io.mem.req.valid := true.B              // Memory request active
      io.mem.req.bits.addr := objPAddr        // Address to write back status word
      io.mem.req.bits.cmd := M_XWR            // Write command
      io.mem.req.bits.data := statusWord      // Data to write (updated status word)
      when(io.mem.req.ready) {                // Wait for memory readiness
        when(io.mem.resp.valid) {             // Confirm write response
        io.mem.req.valid := false.B
          tibVAddr := Cat(statusWord(63, 32), 0.U(32.W)) // Get TIB virtual address from statusWord
          state := loadNumRefs                // Move to state to load number of references
        }
      }
    }

    is(loadNumRefs) {
      // LOAD_NUM_REFS state: Translate TIB virtual address and load number of references
      tlb.io.req.valid := true.B              // Request translation for TIB virtual address
      tlb.io.req.bits.vaddr := tibVAddr       // Set TIB virtual address
      when(tlb.io.req.ready) {                // Wait for TLB readiness
        when(tlb.io.resp.valid) {      
          tlb.io.req.valid := false.B       // Wait for valid TLB response
          val tibPAddr = tlb.io.resp.bits.paddr // Get physical address for TIB
          io.mem.req.valid := true.B          // Memory request active
          io.mem.req.bits.addr := tibPAddr    // Use TIB physical address for memory access
          io.mem.req.bits.cmd := M_XRD        // Read command to retrieve number of references
          when(io.mem.req.ready) {            // Wait for memory readiness
            when(io.mem.resp.valid) {         // Confirm memory response
            io.mem.req.valid := false.B
              numRefs := io.mem.resp.bits.data // Store number of references
              refCounter := 0.U               // Initialize reference counter
              refOffsetsAddr := tibPAddr + 8.U // Set address of first reference offset
              state := processRefs            // Move to process references state
            }
          }
        }
      }
    }

    is(processRefs) {
      io.outputRefReady := false.B
      // PROCESS_REFS state: Load each reference offset and compute reference address
      when(refCounter < numRefs) {            // Continue if there are more references
        io.mem.req.valid := true.B            // Set memory request as valid
        io.mem.req.bits.addr := refOffsetsAddr + (refCounter << 3) // Address of current reference offset
        io.mem.req.bits.cmd := M_XRD          // Read command
        when(io.mem.req.ready) {              // Wait for memory readiness
          when(io.mem.resp.valid) {           // Wait for valid response
          io.mem.req.valid := false.B
            refAddr := io.mem.resp.bits.data + objPAddr // Calculate reference address
            refCounter := refCounter + 1.U    // Increment reference counter
            state := outputRef                // Move to output reference state
          }
        }
      } .otherwise {
        // If all references processed, return to idle state
        state := idle
      }
    }

    is(outputRef) {
      // OUTPUT_REF state: Output the calculated reference address
      io.outputRefReady := true.B             // Indicate reference address is ready
      when(io.outputRefReady) {               // Wait for output acknowledgment
        state := processRefs                  // Return to process next reference
        
      }
    }
  }
}