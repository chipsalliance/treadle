// See LICENSE for license details.

package treadle.executable

import treadle._
import firrtl.{MemKind, WireKind}
import firrtl.ir.{DefMemory, IntWidth}

object Memory {
  //scalastyle:off method.length
  /**
    * Builds all the symbols and dependencies for the specified memory.
    * Pipelines are constructed as registers with a regular name and
    * a /in name.  Data travels up-index through a pipeline for both
    * read and write pipelines.
    * @param memory                  the specified memory
    * @param expandedName            the full name of the memory
    * @param sensitivityGraphBuilder external graph of dependencies
    * @return
    */
  def buildSymbols(
                    memory: DefMemory,
                    expandedName: String,
                    sensitivityGraphBuilder: SensitivityGraphBuilder
  ): Seq[Symbol] = {
    val memorySymbol = Symbol(expandedName, memory.dataType, MemKind, memory.depth)
    val addrWidth = IntWidth(requiredBitsForUInt(memory.depth - 1))
    val addrType  = firrtl.ir.UIntType(addrWidth)

    def buildPipelineDependencies(rootSymbol:      Symbol,
                                  pipelineSymbols: Seq[Symbol],
                                  tailSymbol:      Option[Symbol] = None,
                                  clockSymbol:     Option[Symbol] = None
                                 ): Unit = {

      val chain = Seq(rootSymbol) ++ pipelineSymbols ++ (if(tailSymbol.isDefined) Seq(tailSymbol.get) else Seq.empty)

      clockSymbol.foreach { clock =>
        pipelineSymbols.grouped(2).foreach { x =>
          x.toList match {
            case _ :: register :: Nil =>
              sensitivityGraphBuilder.addSensitivity(clock, register)
            case _ =>
          }
        }
      }

      chain.grouped(2).withFilter(_.length == 2).foreach {
        case source :: target :: Nil =>
          sensitivityGraphBuilder.addSensitivity(source, target)
        case _ =>
      }
      chain.tail.grouped(2).withFilter(_.length == 2).foreach {
        case source :: target :: Nil =>
          sensitivityGraphBuilder.addSensitivity(target, source)
        case _ =>
      }
    }

    val readerSymbols = memory.readers.flatMap { readerString =>
      val readerName = s"$expandedName.$readerString"

      val en   = Symbol(s"$readerName.en",   firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val clk  = Symbol(s"$readerName.clk",  firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val addr = Symbol(s"$readerName.addr", addrType, WireKind)
      val data = Symbol(s"$readerName.data", memory.dataType, WireKind)

      val readerInterfaceSymbols = Seq(en, clk, addr, data)

      sensitivityGraphBuilder.addSensitivity(clk, data)

      val pipelineDataSymbols = (0 until memory.readLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$readerString.pipeline_$n/in", memory.dataType, WireKind),
          Symbol(s"$expandedName.$readerString.pipeline_$n", memory.dataType, WireKind)
        )
      }

      buildPipelineDependencies(addr, pipelineDataSymbols, Some(data))

      readerInterfaceSymbols ++ pipelineDataSymbols
    }

    val writerSymbols = memory.writers.flatMap { writerString =>
      val writerName = s"$expandedName.$writerString"

      val portSymbol = Symbol(writerName, memory.dataType, WireKind)

      val en    = Symbol(s"$writerName.en", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val clk   = Symbol(s"$writerName.clk", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val addr  = Symbol(s"$writerName.addr", addrType, WireKind)
      val mask  = Symbol(s"$writerName.mask", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val data  = Symbol(s"$writerName.data", memory.dataType, WireKind)
      val valid = Symbol(s"$writerName.valid", firrtl.ir.UIntType(IntWidth(1)), WireKind)

      val memoryInterfaceSymbols = Seq(en, clk, addr, mask, data, valid)

      sensitivityGraphBuilder.addSensitivity(clk, portSymbol)
      sensitivityGraphBuilder.addSensitivity(en, valid)
      sensitivityGraphBuilder.addSensitivity(mask, valid)

      val pipelineValidSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$writerString.pipeline_valid_$n/in", firrtl.ir.UIntType(IntWidth(1)), WireKind),
          Symbol(s"$expandedName.$writerString.pipeline_valid_$n", firrtl.ir.UIntType(IntWidth(1)), WireKind)
        )
      }
      buildPipelineDependencies(valid, pipelineValidSymbols, clockSymbol = Some(clk))

      val pipelineDataSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$writerString.pipeline_data_$n/in", memory.dataType, WireKind),
          Symbol(s"$expandedName.$writerString.pipeline_data_$n", memory.dataType, WireKind)
        )
      }
      buildPipelineDependencies(data, pipelineDataSymbols, clockSymbol = Some(clk))

      val pipelineAddrSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$writerString.pipeline_addr_$n/in", addrType, WireKind),
          Symbol(s"$expandedName.$writerString.pipeline_addr_$n", addrType, WireKind)
        )
      }
      buildPipelineDependencies(addr, pipelineAddrSymbols, clockSymbol = Some(clk))

      memoryInterfaceSymbols ++ pipelineValidSymbols ++ pipelineAddrSymbols ++ pipelineDataSymbols ++ Seq(portSymbol)
    }

    val readerWriterSymbols = memory.readwriters.flatMap { readWriterString =>
      val writerName = s"$expandedName.$readWriterString"

      val portSymbol = Symbol(writerName, memory.dataType, WireKind)

      val en    =  Symbol(s"$writerName.en", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val clk   =  Symbol(s"$writerName.clk", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val addr  =  Symbol(s"$writerName.addr", addrType, WireKind)
      val rdata =  Symbol(s"$writerName.rdata", memory.dataType, WireKind)
      val mode  =  Symbol(s"$writerName.wmode", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val mask  =  Symbol(s"$writerName.wmask", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val wdata =  Symbol(s"$writerName.wdata", memory.dataType, WireKind)
      val valid =  Symbol(s"$writerName.valid", firrtl.ir.UIntType(IntWidth(1)), WireKind)

      val memoryInterfaceSymbols = Seq(en, clk, addr, rdata, mode, mask, wdata, valid)

      sensitivityGraphBuilder.addSensitivity(clk, portSymbol)
      sensitivityGraphBuilder.addSensitivity(clk, rdata)
      sensitivityGraphBuilder.addSensitivity(en, valid)
      sensitivityGraphBuilder.addSensitivity(mode, valid)
      sensitivityGraphBuilder.addSensitivity(mask, valid)

      val pipelineReadDataSymbols = (0 until memory.readLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$readWriterString.pipeline_rdata_$n/in", memory.dataType, WireKind),
          Symbol(s"$expandedName.$readWriterString.pipeline_rdata_$n", memory.dataType, WireKind)
        )
      }
      buildPipelineDependencies(addr, pipelineReadDataSymbols, Some(rdata), clockSymbol = Some(clk))

      val pipelineEnableSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$readWriterString.pipeline_valid_$n/in", firrtl.ir.UIntType(IntWidth(1)), WireKind),
          Symbol(s"$expandedName.$readWriterString.pipeline_valid_$n", firrtl.ir.UIntType(IntWidth(1)), WireKind)
        )
      }
      buildPipelineDependencies(valid, pipelineEnableSymbols, clockSymbol = Some(clk))

      val pipelineWriteDataSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$readWriterString.pipeline_wdata_$n/in", memory.dataType, WireKind),
          Symbol(s"$expandedName.$readWriterString.pipeline_wdata_$n", memory.dataType, WireKind)
        )
      }
      buildPipelineDependencies(wdata, pipelineWriteDataSymbols, clockSymbol = Some(clk))

      val pipelineAddrSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$readWriterString.pipeline_addr_$n/in", addrType, WireKind),
          Symbol(s"$expandedName.$readWriterString.pipeline_addr_$n", addrType, WireKind)
        )
      }
      buildPipelineDependencies(addr, pipelineAddrSymbols, clockSymbol = Some(clk))

      memoryInterfaceSymbols    ++
        pipelineReadDataSymbols ++
        pipelineEnableSymbols   ++
        pipelineAddrSymbols     ++
        pipelineWriteDataSymbols ++
        Seq(portSymbol)
    }

    Seq(memorySymbol) ++ readerSymbols ++ writerSymbols ++ readerWriterSymbols
  }

  /**
    * Construct the machinery to move data into and out of the memory stack
    * @param memory       current memory
    * @param expandedName full path name
    * @param scheduler    handle to execution components
    * @param compiler     needed for assigner generation
    */
  def buildMemoryInternals(
                            memory: DefMemory, expandedName: String, scheduler: Scheduler, compiler: ExpressionCompiler
                          ): Unit = {
    val symbolTable  = scheduler.symbolTable
    val memorySymbol = symbolTable(expandedName)
    val dataStore    = compiler.dataStore

    /*
      * construct a pipeline of registers based on the latency
      * @param portString   reader or writer port name
      * @param pipelineName name of data being pipelined
      * @param latency      length of pipeline
      * @return
      */
    def buildPipeLine(portString: String, pipelineName: String, latency: Int): Seq[Symbol] = {
      (0 until latency).flatMap { n =>
        Seq(
          symbolTable(s"$portString.pipeline_${pipelineName}_$n/in"),
          symbolTable(s"$portString.pipeline_${pipelineName}_$n")
        )
      }
    }

    /*
      * Makes a read chain of pipeline registers.  These must be ordered reg0/in, reg0, reg1/in ... regN/in, regN
      * This will advance the registers on the specified clock,
      * and combinationally pass the register value to the next register's input down the chain
      * Data flows from low indexed pipeline elements to high ones

      * @param clock          trigger
      * @param portName       port name
      * @param pipelineName   element being pipelined
      * @param data           data where memory data will go
      * @param addr           address of data in memory
      * @param enable         memory enabled
      */
    def buildReadPipelineAssigners(
                                    clock:        Symbol,
                                    portName:     String,
                                    pipelineName: String,
                                    data:         Symbol,
                                    addr:         Symbol,
                                    enable:       Symbol
                                  ): Unit = {

      val pipelineReadSymbols = buildPipeLine(portName, pipelineName, memory.readLatency)
      val chain = Seq() ++ pipelineReadSymbols ++ Seq(data)

      compiler.makeAssigner(chain.head, compiler.makeGetIndirect(memorySymbol, data, enable, addr))

      compiler.makeAssigner(chain.head, compiler.makeGetIndirect(memorySymbol, data, enable, addr))

      // This produces triggered: reg0 <= reg0/in, reg1 <= reg1/in etc.
      chain.grouped(2).withFilter(_.length == 2).toList.foreach {
        case source :: target :: Nil =>
          compiler.makeAssigner(target, compiler.makeGet(source))
        case _ =>
      }

      // This produces reg0/in <= root, reg1/in <= reg0 etc.
      chain.drop(1).grouped(2).withFilter(_.length == 2).toList.foreach {
        case source :: target :: Nil =>
          compiler.makeAssigner(target, compiler.makeGet(source))
        case _ =>
      }
    }

    memory.readers.foreach { readerString =>
      val readerName = s"$expandedName.$readerString"
      val enable = symbolTable(s"$readerName.en")
      val clock  = symbolTable(s"$readerName.clk")
      val addr   = symbolTable(s"$readerName.addr")
      val data   = symbolTable(s"$readerName.data")

      buildReadPipelineAssigners(clock, readerName, "data", data, addr, enable)
    }

    /*
      * compile the necessary assignments to complete a latency chain
      * If latency is zero, this basically returns the root memorySymbol.
      * @param clockSymbol   used to create execution based on this trigger.
      * @param rootSymbol    the head element of the pipeline, this is one of the memort ports
      * @param writerString  name of the writer
      * @param pipelineName  string representing the name of the root port
      * @return
      */
    def buildWritePipelineAssigners(clockSymbol:     Symbol,
                                    rootSymbol:      Symbol,
                                    writerString:    String,
                                    pipelineName:    String
                                   ): Symbol = {

      val pipelineSymbols = buildPipeLine(writerString, pipelineName, memory.writeLatency)
      val chain = Seq(rootSymbol) ++ pipelineSymbols

      // This produces triggered: reg0 <= reg0/in, reg1 <= reg1/in etc.
      chain.drop(1).grouped(2).withFilter(_.length == 2).toList.foreach {
        case source :: target :: Nil =>
          compiler.makeAssigner(target, compiler.makeGet(source))
        case _ =>
      }

      // This produces reg0/in <= root, reg1/in <= reg0 etc.
      chain.grouped(2).withFilter(_.length == 2).toList.foreach {
        case source :: target :: Nil =>
          compiler.makeAssigner(target, compiler.makeGet(source))
        case _ =>
      }

      chain.last
    }

    memory.writers.foreach { writerString =>
      val writerName = s"$expandedName.$writerString"

      val portSymbol = symbolTable(writerName)

      val enable = symbolTable(s"$writerName.en")
      val clock  = symbolTable(s"$writerName.clk")
      val addr   = symbolTable(s"$writerName.addr")
      val mask   = symbolTable(s"$writerName.mask")
      val data   = symbolTable(s"$writerName.data")
      val valid  = symbolTable(s"$writerName.valid")

      // compute a valid so we only have to carry a single boolean up the write queue
      compiler.makeAssigner(valid, AndInts(dataStore.GetInt(enable.index).apply, dataStore.GetInt(mask.index).apply, 1))

      val endOfValidPipeline = buildWritePipelineAssigners(clock, valid, writerName, "valid")
      val endOfAddrPipeline  = buildWritePipelineAssigners(clock, addr, writerName, "addr")
      val endOfDataPipeline  = buildWritePipelineAssigners(clock, data, writerName, "data")

      compiler.makeIndirectAssigner(
        portSymbol,
        memorySymbol,
        endOfAddrPipeline,
        endOfValidPipeline,
        compiler.makeGet(endOfDataPipeline),
        clock
      )
    }

    memory.readwriters.foreach { readWriterString =>
      val writerName = s"$expandedName.$readWriterString"

      val portSymbol = symbolTable(writerName)

      val enable = symbolTable(s"$writerName.en")
      val clock  = symbolTable(s"$writerName.clk")
      val addr   = symbolTable(s"$writerName.addr")
      val rdata  = symbolTable(s"$writerName.rdata")
      val mode   = symbolTable(s"$writerName.wmode")
      val mask   = symbolTable(s"$writerName.wmask")
      val wdata  = symbolTable(s"$writerName.wdata")
      val valid  = symbolTable(s"$writerName.valid")

      buildReadPipelineAssigners(clock, writerName, "rdata", rdata, addr, enable)

      // compute a valid so we only have to carry a single boolean up the write queue
      compiler.makeAssigner(
        valid,
        AndInts(
          AndInts(dataStore.GetInt(enable.index).apply, dataStore.GetInt(mask.index).apply, 1).apply,
          dataStore.GetInt(mode.index).apply,
          1
        )
      )

      val endOfValidPipeline = buildWritePipelineAssigners(clock, valid, writerName, "valid")
      val endOfAddrPipeline  = buildWritePipelineAssigners(clock, addr,  writerName, "addr")
      val endOfDataPipeline  = buildWritePipelineAssigners(clock, wdata, writerName, "wdata")

      compiler.makeIndirectAssigner(
        portSymbol,
        memorySymbol,
        endOfAddrPipeline,
        endOfValidPipeline,
        compiler.makeGet(endOfDataPipeline),
        clock
      )
    }
  }


}
