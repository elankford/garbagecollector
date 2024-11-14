package rocket

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import firrtl.{AnnotationSeq, CustomDefaultMemoryEmission, CustomDefaultRegisterEmission, MemoryNoInit}
import freechips.rocketchip.devices.debug.{DebugModuleKey, DefaultDebugModuleParams}
import freechips.rocketchip.devices.tilelink.{BootROMLocated, BootROMParams, BuiltInErrorDeviceParams, CLINTKey, CLINTParams, DevNullParams, PLICKey, PLICParams}
import freechips.rocketchip.diplomacy.{AddressSet, MonitorsEnabled}
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem.{CacheBlockBytes, ControlBusKey, FrontBusKey, FrontBusParams, InSubsystem, JustOneBusTopologyParams, MemoryBusKey, MemoryBusParams, PeripheryBusKey, PeripheryBusParams, SubsystemExternalResetVectorKey, SystemBusKey, SystemBusParams, TLNetworkTopologyLocated}
import freechips.rocketchip.tile.{MaxHartIdBits, RocketTileParams, TileKey, XLen}

class GCWrapper(markQueueDepth: Int = 5000)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    // Signals to start and stop GC
    val gcStart = Input(Bool()) // Signal to start garbage collection
    val gcDone = Output(Bool()) // Signal that GC is done

    // root objects address
    val gcAddress = Input(UInt(64.W)) // Base address of the root objects

    val sweepAddress = Input(UInt(64.W)) // Base address of the blocks to be looked at by the Sweeper

    // Interface with Memory
    val mem = new HellaCacheIO // TileLink memory interface
  })

  // Internal state definitions for marker FSM
  val idle :: loadRootCount :: loadRoots :: popMarkQueue :: Nil = Enum(4)
  val state = RegInit(idle)

  val rootPTR = Reg(UInt(64.W)) // Root pointer to be loaded into mark queue

  // Interface to Marker
  val markerOutputDoneProcessing = Wire(Bool())
  val markVAddr = Reg(UInt(64.W)) // Virtual address from the mark queue
  val markQueueDataReady = Wire(Bool()) // Signal indicating data is ready for marker

  // Connect Marker signals
  marker.io.markQueueVAddrIn := markVAddr
  marker.io.markQueueValidIn := markQueueDataReady
  markerOutputDoneProcessing := marker.io.outputDoneProcessing
  markerOutputAddr := marker.io.outboundRefAddrOut
  markerOutputRefReady := marker.io.outputRefReady

  // Internal registers
  val rootCount = Reg(UInt(64.W)) // Number of roots to load
  val rootCounter = RegInit(0.U(64.W)) // Counter for iterating through roots

  // Instantiate the Marker module
  val marker = Module(new Marker)

  // Interface to Tracer
  val tracerVAddr = Reg(UInt(64.W)) // Address for tracer to process
  val tracerQueueDataReady = Wire(Bool()) // Signal indicating data for tracer is ready
  val toBeMarked = Wire(Bool()) // Tracer indicates if an address needs to be added to mark queue
  val toBeMarkedData = Reg(UInt(64.W)) // Data to be added to mark queue
  val tracerReadyForNext = Wire(Bool()) // Tracer ready for next address


  // Internal state definitions for tracer FSM
  val idle :: waitForTracer :: enqueueToMarkQueue :: Nil = Enum(3)
  val tracerState = RegInit(idle)

  //THESE ARE FOR THE TRACER QUEUE
  val markerOutputAddr = Wire(UInt(64.W)) // Output address from the marker
  val markerOutputRefReady = Wire(Bool()) // Signal that a reference address is ready

  // Connect Tracer signals
  tracer.io.tracerQueueVAddrIn := tracerVAddr
  tracer.io.tracerQueueDataReady := tracerQueueDataReady
  toBeMarked := tracer.io.toBeMarked
  tracerReadyForNext := tracer.io.tracerReadyForNext
  toBeMarkedData := tracer.io.toBeMarkedData

  // Instantiate the Tracer module
  val tracer = Module(new Tracer)


  //marktrace signals for sweeper
  val markdone = RegInit(Bool(false.B))


  //SWEEPER WIRES, REGS, STATES
  // Instantiate the Sweeper module
  val sweeper = Module(new Sweeper)

  // Connect Sweeper signals
  sweeper.io.blockPtrReady := blockPtrReady
  sweeperReady := sweeper.io.sweeperReady
  val blockAddr = Reg(UInt(64.W)) // Block address that is popped from sweeperqueue
  sweeper.io.blockAddr := blockAddr

  // Internal registers for sweeper
  val numBlocks = Reg(UInt(64.W)) // Number of live blocks
  val blockCount = RegInit(0.U(64.W)) // Counter for iterating through blocks
  val currentAddr = Reg(UInt(64.W)) // Current address for memory read

  //Interface to Sweeper
  val blockPtrReady = RegInit(Bool(false.B))
  val sweeperReady = Wire(Bool())

  // Internal state definitions for sweeper FSM
  val init :: readNumBlocks :: readBlockPtr :: loadSweeperQueue :: setupDone :: idle :: waitForSweep :: Nil = Enum(7)
  val sweeperState = RegInit(idle)


  //WE NEED TO PUSH THE MARKEROUTPTADDR INTO THE TRACE QUEUE WHENEVER MARKEROUTPUTREFREADY IS HIGH
  when(markerOutputRefReady) {
    traceQueue.io.enq.valid := true.B
    traceQueue.io.enq.bits := markerOutputAddr
  }

  // Default memory interface signals
  io.mem.req.valid := false.B
  io.mem.req.bits.cmd := M_XRD
  io.mem.req.bits.size := log2Ceil(8).U // 64-bit reads
  io.mem.req.bits.addr := 0.U


  //ALL OF QUEUES
  // Mark queue
  val markQueue = Module(new Queue(UInt(64.W), markQueueDepth))
  markQueue.io.enq.valid := false.B
  markQueue.io.enq.bits := 0.U

  // Trace queue
  val traceQueue = Module(new Queue(UInt(64.W), markQueueDepth))
  traceQueue.io.enq.valid := false.B
  traceQueue.io.enq.bits := 0.U

  // Sweep queue
  val sweeperQueue = Module(new Queue(UInt(64.W), markQueueDepth))
  sweeperQueue.io.enq.valid := false.B
  sweeperQueue.io.enq.bits := 0.U



  //this happens when we start a new GC
  when(markDone === 0) {


    // State machine to manage GC initialization and mark queue
    switch(state) {
      is(idle) {
        when(io.gcStart) {
          currentAddr := io.sweepAddress
          rootCounter := 0.U
          rootPTR := io.gcAddress
          state := loadRootCount
        }
      }

      is(loadRootCount) {
        // Load the first 64-bit word to determine the number of roots
        io.mem.req.valid := true.B
        io.mem.req.bits.addr := rootPTR
        io.mem.req.bits.cmd := M_XRD
        when(io.mem.req.ready) {
          when(io.mem.resp.valid) {
            io.mem.req.valid := false.B
            rootCount := io.mem.resp.bits.data
            rootCounter := 0.U
            state := loadRoots
          }
        }
      }

      is(loadRoots) {
        // Load each root address and enqueue it into the mark queue
        io.mem.req.valid := rootCounter < rootCount
        io.mem.req.bits.addr := rootPTR + (8.U * (rootCounter + 1.U)) // Address offset by 8 bytes per root
        io.mem.req.bits.cmd := M_XRD
        when(io.mem.req.ready) {
          when(io.mem.resp.valid && markQueue.io.enq.ready) {
            io.mem.req.valid := false.B
            markQueue.io.enq.valid := true.B
            markQueue.io.enq.bits := io.mem.resp.bits.data
            rootCounter := rootCounter + 1.U
            when(rootCounter === rootCount - 1.U) {
              state := popMarkQueue
            }
          }
        }
      }

      is(popMarkQueue) {

        // Pop from the mark queue when data is ready to send to the marker
        when(markQueue.io.deq.valid && markerOutputDoneProcessing) {
          markQueue.io.deq.ready := true.B
          markVAddr := markQueue.io.deq.bits
          markQueueDataReady := true.B
        }.elsewhen(!markerOutputDoneProcessing || !markQueue.io.deq.valid) {
          markQueue.io.deq.ready := false.B
        }
      }
    }

    //State machine to manage our tracer
    switch(tracerState) {
      is(idle) {
        tracerQueue.io.deq.ready := false.B
        when(tracerQueue.io.deq.valid && tracerReadyForNext) {
          // Pop from tracer queue and send address to tracer
          tracerQueueDataReady := true.B
          tracerVAddr := tracerQueue.io.deq.bits
          tracerQueue.io.deq.ready := true.B
          tracerState := waitForTracer

        }
      }

      is(waitForTracer) {
        when(toBeMarked) {
          // If the tracer returned data is valid and marked, move to enqueue state
          markQueue.io.enq.valid := true.B
          markQueue.io.enq.bits := toBeMarkedData
        }
        when(tracerReadyForNext) {
          // If tracer is ready for next address, move to idle state
          markQueue.io.enq.valid := false.B
          tracerState := idle
        }

      }

      // GC done condition (if both queues are empty and FSMs are idle)
      markdone := (state ==== popMarkQueue && tracerState === idle && tracerReadyForNext === true.B && markerOutputDoneProcessing === true.B && markQueue.isEmpty && tracerQueue.isEmpty)
    }


  }
  //SWEEPER SECTION

  //Grab all block pointers from memory address that is written to by allocator
  //put all of these pointers into sweeperqueue
  //while queue is not empty, pop off the queue and send address into sweeper, set blockptrReady to 1
  //sweeper iterates through cells then sets sweeperReady to 1
  //whole loop continues until sweeperReady ==1 and sweeperqueue is empty

  // FSM Logic
  switch(sweeperState) {
    is(idle) {

      when(markdone) { // We remain in idle until the marktrace phase is done
        // Reset counters and initialize address for first read
        blockCount := 0.U
        io.mem.req.bits.addr := currentAddr
        io.mem.req.valid := true.B // Initiate first memory read
        when(io.mem.req.ready) {
          state := readNumBlocks
        }
      }
    }

    is(readNumBlocks) {
      // Read number of live blocks from the first memory address

      when(io.mem.resp.valid) {
        io.mem.req.valid := false.B
        numBlocks := io.mem.resp.bits.data // Store the number of blocks
        currentAddr := currentAddr + 8.U // Move to first block pointer (8 bytes offset)
        state := readBlockPtr
      }
    }

    is(readBlockPtr) {
      when(blockCount < numBlocks) {
        io.mem.req.valid := true.B // Start read for each block pointer
        io.mem.req.bits.addr := currentAddr // Set current address

        when(io.mem.req.ready) {
          state := loadSweeperQueue
        }
      }.otherwise {
        state := setupDone // All block pointers read
      }
    }

    is(loadSweeperQueue) {
      when(io.mem.resp.valid) {
        io.mem.req.valid := false.B
        // Push block pointer into `sweeperQueue`
        when(sweeperQueue.io.enq.ready) {
          sweeperQueue.io.enq.valid := true.B
          sweeperQueue.io.enq.bits := io.mem.resp.bits.data // Enqueue block pointer
          blockCount := blockCount + 1.U
          currentAddr := currentAddr + 8.U // Move to the next block pointer
          state := readBlockPtr
        }
      }
    }

    is(setupDone) {
      when(sweeperQueue.io.deq.valid && sweeperReady) {
        // Dequeue block pointer and signal sweeper module
        sweeperQueue.io.deq.ready := true.B
        blockPtrReady := true.B
        blockAddr := sweeperrQueue.io.deq.bits

        state := waitForSweep // Return to idle once queue is empty
      }
      when(sweeperQueue.isEmpty) {
        // If queue is empty, move to idle state
        state := idle
      }
    }

    is(waitForSweep) {
      blockPtrReady := false.B
      sweeperQueue.io.deq.ready := false.B
      when(sweeperReady) {
        // If sweeper is ready, move to idle state
        state := setupDone
      }
    }

    is(idle) {
      // done with everything
      io.gcDone := true.B
      markdone := false.B
    }
  }
  // very end of sweeping - markdone low and everything done to high
}

object GCWrapperDriver {
  def main(args: Array[String]): Unit = (new ChiselStage).emitVerilog(new GCWrapper()(Parameters((site, here, up) => {
    case PgLevels => 5
    case XLen => 64 // Applies to all cores
    case MaxHartIdBits => 1
    // Interconnect parameters
    case SystemBusKey =>
      SystemBusParams(
        beatBytes = site(XLen) / 8,
        blockBytes = site(CacheBlockBytes)
      )
    case ControlBusKey => //noinspection DuplicatedCode
      PeripheryBusParams(
        beatBytes = site(XLen) / 8,
        blockBytes = site(CacheBlockBytes),
        errorDevice = Some(
          BuiltInErrorDeviceParams(
            errorParams = DevNullParams(
              List(AddressSet(0x3000, 0xfff)),
              maxAtomic = site(XLen) / 8,
              maxTransfer = 4096
            )
          )
        )
      )
    case PeripheryBusKey =>
      PeripheryBusParams(
        beatBytes = site(XLen) / 8,
        blockBytes = site(CacheBlockBytes),
        dtsFrequency = Some(100000000)
      ) // Default to 100 MHz pbus clock
    case MemoryBusKey =>
      MemoryBusParams(
        beatBytes = site(XLen) / 8,
        blockBytes = site(CacheBlockBytes)
      )
    case FrontBusKey =>
      FrontBusParams(
        beatBytes = site(XLen) / 8,
        blockBytes = site(CacheBlockBytes)
      )
    // Additional device Parameters
    case BootROMLocated(InSubsystem) =>
      Some(BootROMParams(contentFileName = "./bootrom/bootrom.img"))
    case SubsystemExternalResetVectorKey => false
    case DebugModuleKey => Some(DefaultDebugModuleParams(site(XLen)))
    case CLINTKey => Some(CLINTParams())
    case PLICKey => Some(PLICParams())
    // Copying WithJustOneBus
    case TLNetworkTopologyLocated(InSubsystem) =>
      List(
        JustOneBusTopologyParams(sbus = site(SystemBusKey))
      )
    case MonitorsEnabled => false
    case TileKey => RocketTileParams()
  })),
    annotations = AnnotationSeq(Seq(
      CustomDefaultMemoryEmission(MemoryNoInit),
      CustomDefaultRegisterEmission(useInitAsPreset = false, disableRandomization = true)
    )))

}




    







 

  

