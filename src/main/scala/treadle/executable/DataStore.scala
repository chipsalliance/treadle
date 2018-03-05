// See LICENSE for license details.

package treadle.executable

import treadle.vcd.VCD
import treadle.{BlackBoxImplementation, ExecutionEngine, TreadleException}
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

import scala.collection.mutable

/**
  * Creates a data store for the three underlying data types.
  * The numberOfBuffers is used to control the ability to rollback execution.
  * The meaning of the values of each slot must be maintained outside of this class.
  * This class only supports (2 ** 31) - 1 of any ints, longs or bigs.
  *
  * @param numberOfBuffers Number of buffers
  */
//scalastyle:off number.of.methods
class DataStore(val numberOfBuffers: Int, optimizationLevel: Int = 0) {
  assert(numberOfBuffers > 0, s"DataStore: numberOfBuffers $numberOfBuffers must be > 0")

  private val nextIndexFor = new mutable.HashMap[DataSize, Int]
  nextIndexFor(IntSize)  = 0
  nextIndexFor(LongSize) = 0
  nextIndexFor(BigSize)  = 0

  private var executionEngineOption: Option[ExecutionEngine] = None
  def setExecutionEngine(executionEngine: ExecutionEngine): Unit = {
    executionEngineOption = Some(executionEngine)

    executionEngine.optionsManager.treadleOptions.symbolsToWatch.foreach { symbolName =>
      if (executionEngine.symbolTable.contains(symbolName)) {
        watchList += executionEngine.symbolTable(symbolName)
      }
      else {
        throw TreadleException(s"treadleOptions.symbols to watch has bad symbolName $symbolName")
      }
    }

    setAssignmentDisplayModes()
  }

  def setAssignmentDisplayModes(): Unit = {
    executionEngineOption.foreach { executionEngine =>
      val watchList = executionEngine.optionsManager.treadleOptions.symbolsToWatch.map { symbolName =>
        executionEngine.symbolTable.get(symbolName) match {
          case Some(symbol) =>
            symbol
          case _ =>
            throw TreadleException(s"treadleOptions.symbols to watch has bad symbolName $symbolName")
        }
      }

      val verbose = executionEngineOption.get.verbose
      executionEngine.scheduler.activeAssigns.foreach { assigner =>
        val render = watchList.contains(assigner.symbol)
        assigner.setLeanMode(!verbose && !render)
        assigner.setVerbose(verbose)
        assigner.setRender(render)
      }
    }
  }

  def numberOfInts: Int  = nextIndexFor(IntSize)
  def numberOfLongs: Int = nextIndexFor(LongSize)
  def numberOfBigs: Int  = nextIndexFor(BigSize)

  val watchList: mutable.HashSet[Symbol] = new mutable.HashSet()

  var vcdOption: Option[VCD] = None

  def vcdUpdate(symbol: Symbol, value: BigInt): Unit = {
    if(vcdOption.isDefined) {
      vcdOption.get.wireChanged(symbol.name, value, width = symbol.bitWidth)
    }
  }
  def getSizes: (Int, Int, Int) = {
    (nextIndexFor(IntSize), nextIndexFor(LongSize), nextIndexFor(BigSize))
  }

  def getIndex(dataSize: DataSize, slots: Int = 1): Int = {
    val index = nextIndexFor(dataSize)
    nextIndexFor(dataSize) += slots
    index
  }

  var intData:  Array[Array[Int]]  = Array.fill(numberOfBuffers, numberOfInts)(0)
  var longData: Array[Array[Long]] = Array.fill(numberOfBuffers, numberOfLongs)(0L)
  var bigData:  Array[Array[Big]]  = Array.fill(numberOfBuffers, numberOfBigs)(Big(0))

  var currentBufferIndex:  Int = if(numberOfBuffers > 1) 1 else 0
  var previousBufferIndex: Int = 0

  var currentIntArray:   Array[Int]  = intData(currentBufferIndex)
  var currentLongArray:  Array[Long] = longData(currentBufferIndex)
  var currentBigArray:   Array[Big]  = bigData(currentBufferIndex)
  var previousIntArray:  Array[Int]  = intData(previousBufferIndex)
  var previousLongArray: Array[Long] = longData(previousBufferIndex)
  var previousBigArray:  Array[Big]  = bigData(previousBufferIndex)

  def allocateBuffers(): Unit = {
    intData  = Array.fill(numberOfBuffers, numberOfInts)(0)
    longData = Array.fill(numberOfBuffers, numberOfLongs)(0L)
    bigData  = Array.fill(numberOfBuffers, numberOfBigs)(Big(0))

    currentIntArray  = intData(currentBufferIndex)
    currentLongArray = longData(currentBufferIndex)
    currentBigArray  = bigData(currentBufferIndex)
    previousIntArray  = intData(previousBufferIndex)
    previousLongArray = longData(previousBufferIndex)
    previousBigArray  = bigData(previousBufferIndex)
  }

  /**
    * Get the three source buffers
    * @return
    */
  def sourceBuffers(): (Array[Int], Array[Long], Array[Big]) = {
    (intData(previousBufferIndex), longData(previousBufferIndex), bigData(previousBufferIndex))
  }

  /**
    * Get the three target buffers
    * @return
    */
  def targetBuffers(): (Array[Int], Array[Long], Array[Big]) = {
    (intData(currentBufferIndex), longData(currentBufferIndex), bigData(currentBufferIndex))
  }

  /**
    * Advance the buffers if you are using more than 1.
    * TODO: When the heck do you call this thing
    */
  def advanceBuffers(): Unit = {
    if(numberOfBuffers > 1) {
      previousBufferIndex = (previousBufferIndex + 1) % numberOfBuffers
      currentBufferIndex = (currentBufferIndex + 1) % numberOfBuffers

      currentIntArray  = intData(currentBufferIndex)
      currentLongArray = longData(currentBufferIndex)
      currentBigArray  = bigData(currentBufferIndex)

      previousIntArray  = intData(previousBufferIndex)
      previousLongArray = longData(previousBufferIndex)
      previousBigArray  = bigData(previousBufferIndex)

      for(i <- currentIntArray.indices)  { currentIntArray(i)  = previousIntArray(i) }
      for(i <- currentLongArray.indices) { currentLongArray(i) = previousLongArray(i) }
      for(i <- currentBigArray.indices)  { currentBigArray(i)  = previousBigArray(i) }

      // println(s"dataStore:advanceBuffers:current $currentBufferIndex previous $previousBufferIndex")
    }
  }

  def showAssignment(symbol: Symbol): Unit = {
    val showValue = symbol.normalize(apply(symbol))
    println(s"${symbol.name} <= $showValue h${showValue.toString(16)}")
  }

  def showIndirectAssignment(symbol: Symbol, value: BigInt, index: Int): Unit = {
    //TODO (chick) Need to build in display of index computation
    val showValue = symbol.normalize(value)
    println(s"${symbol.name}($index) <= $showValue")
  }

  def renderAssignment(symbol: Symbol): Unit = {
    executionEngineOption.foreach { executionEngine =>
      println(executionEngine.renderComputation(symbol.name))
    }
  }

  case class GetInt(index: Int) extends IntExpressionResult {
    def apply(): Int = currentIntArray(index)
  }

  case class AssignInt(symbol: Symbol, expression: FuncInt) extends Assigner {
    val index: Int = symbol.index

    def runLean(): Unit = {currentIntArray(index) = expression() }

    def runFull(): Unit = {
      val value = expression()
      currentIntArray(index) = value
      vcdUpdate(symbol, value)
      if(isVerbose) showAssignment(symbol)
      if(doRender) renderAssignment(symbol)
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if(isLean) runLean _ else runFull _
    }
    var run: FuncUnit = if(optimizationLevel == 0) runFull _ else runLean _
  }

  case class PosEdgeAssignInt(symbol: Symbol, clockExpression: FuncInt, expression: FuncInt) extends Assigner {
    val index: Int = symbol.index

    var lastClockValue = clockExpression()

    def runLean(): Unit = {
      val clockValue = clockExpression()
      if(clockValue > 0 && lastClockValue == 0) {
        currentIntArray(index) = expression()
      }
      lastClockValue = clockValue
    }

    def runFull(): Unit = {
      val value = expression()
      val clockValue = clockExpression()
      if(clockValue > 0 && lastClockValue == 0) {
        currentIntArray(index) = expression()
      }
      lastClockValue = clockValue

      vcdUpdate(symbol, value)
      if(isVerbose) showAssignment(symbol)
      if(doRender) renderAssignment(symbol)
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if(isLean) runLean _ else runFull _
    }
    var run: FuncUnit = if(optimizationLevel == 0) runFull _ else runLean _
  }

  case class GetLong(index: Int) extends LongExpressionResult {
    def apply(): Long = currentLongArray(index)
  }

  case class AssignLong(symbol: Symbol, expression: FuncLong) extends Assigner {
    val index: Int = symbol.index

    def runLean(): Unit = {
      currentLongArray(index) = expression()
    }

    def runFull(): Unit = {
      val value = expression()
      currentLongArray(index) = value
      vcdUpdate(symbol, value)
      if(isVerbose) showAssignment(symbol)
      if(doRender) renderAssignment(symbol)
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if(isLean) {
        runLean _
      } else {
        runFull _
      }
    }
    var run: FuncUnit = if(optimizationLevel == 0) runFull _ else runLean _
  }

  case class PosEdgeAssignLong(symbol: Symbol, clockExpression: FuncInt, expression: FuncLong) extends Assigner {
    val index: Int = symbol.index

    var lastClockValue = clockExpression()

    def runLean(): Unit = {
      val clockValue = clockExpression()

      if(clockValue > 0 && lastClockValue == 0) {
        currentLongArray(index) = expression()
      }
      lastClockValue = clockValue
    }

    def runFull(): Unit = {
      val value = expression()
      val clockValue = clockExpression()

      if(clockValue > 0 && lastClockValue == 0) {
        currentLongArray(index) = value
      }
      lastClockValue = clockValue

      vcdUpdate(symbol, value)
      if(isVerbose) showAssignment(symbol)
      if(doRender) renderAssignment(symbol)
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if(isLean) runLean _ else runFull _
    }
    var run: FuncUnit = if(optimizationLevel == 0) runFull _ else runLean _
  }


  case class GetBig(index: Int) extends BigExpressionResult {
    def apply(): Big = currentBigArray(index)
  }

  case class AssignBig(symbol: Symbol, expression: FuncBig) extends Assigner {
    val index: Int = symbol.index

    def runLean(): Unit = {
      currentBigArray(index) = expression()
    }
    def runFull(): Unit = {
      val value = expression()
      currentBigArray(index) = value
      vcdUpdate(symbol, value)
      if(isVerbose) showAssignment(symbol)
      if(doRender) renderAssignment(symbol)
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if(isLean) runLean _ else runFull _
    }
    var run: FuncUnit = if(optimizationLevel == 0) runFull _ else runLean _
  }

  case class PosEdgeAssignBig(symbol: Symbol, clockExpression: FuncInt, expression: FuncBig) extends Assigner {
    val index: Int = symbol.index

    var lastClockValue = clockExpression()

    def runLean(): Unit = {
      val clockValue = clockExpression()
      if(clockValue > 0 && lastClockValue == 0) {
        currentBigArray(index) = expression()
      }
      lastClockValue = clockValue
    }

    def runFull(): Unit = {
      val value = expression()
      val clockValue = clockExpression()
      if(clockValue > 0 && lastClockValue == 0) {
        currentBigArray(index) = value
      }
      lastClockValue = clockValue

      vcdUpdate(symbol, value)
      if(isVerbose) showAssignment(symbol)
      if(doRender) renderAssignment(symbol)
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if(isLean) runLean _ else runFull _
    }
    var run: FuncUnit = if(optimizationLevel == 0) runFull _ else runLean _
  }


  /** for memory implementations */
  case class GetIntIndirect(
                             memorySymbol: Symbol,
                             getMemoryIndex: FuncInt,
                             enable: FuncInt
                           ) extends IntExpressionResult {
    val memoryLocation: Int = memorySymbol.index
    def apply(): Int = {
      currentIntArray(memoryLocation + (getMemoryIndex() % memorySymbol.slots))
    }
  }

  case class GetLongIndirect(
                             memorySymbol: Symbol,
                             getMemoryIndex: FuncInt,
                             enable: FuncInt
                           ) extends LongExpressionResult {
    val memoryLocation: Int = memorySymbol.index
    def apply(): Long = {
      currentLongArray(memoryLocation + (getMemoryIndex() % memorySymbol.slots))
    }
  }

  case class GetBigIndirect(
                             memorySymbol: Symbol,
                             getMemoryIndex: FuncInt,
                             enable: FuncInt
                           ) extends BigExpressionResult {
    val memoryLocation: Int = memorySymbol.index
    def apply(): Big = {
      currentBigArray(memoryLocation + (getMemoryIndex() % memorySymbol.slots))
    }
  }

  case class AssignIntIndirect(
                               symbol: Symbol,
                               memorySymbol: Symbol,
                               getMemoryIndex: FuncInt,
                               enable: FuncInt,
                               expression: FuncInt
                              ) extends Assigner {
    val index: Int = memorySymbol.index

    def runLean(): Unit = {
      if(enable() > 0) {
        currentIntArray(index + getMemoryIndex.apply()) = expression()
      }
    }

    def runFull(): Unit = {
      if(enable() > 0) {
        val value = expression()
        val memoryIndex = getMemoryIndex.apply()
        currentIntArray(index + (memoryIndex % memorySymbol.slots)) = value
        vcdUpdate(symbol, value)
        if(isVerbose) showIndirectAssignment(symbol, value, memoryIndex)
      }
      else {
        if(isVerbose) {
          println(s"${symbol.name}(${getMemoryIndex.apply() % memorySymbol.slots}) <= NOT ENABLED")
        }
      }
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if(isLean) runLean _ else runFull _
    }
    var run: FuncUnit = if (optimizationLevel == 0) runFull _ else runLean _
  }

  case class AssignLongIndirect(
                               symbol: Symbol,
                               memorySymbol: Symbol,
                               getMemoryIndex: FuncInt,
                               enable: FuncInt,
                               expression: FuncLong
                              ) extends Assigner {
    val index: Int = memorySymbol.index

    def runLean(): Unit = {
      if(enable() > 0) {
        currentLongArray(index + (getMemoryIndex.apply() % memorySymbol.slots)) = expression()
      }
    }

    def runFull(): Unit = {
      if(enable() > 0) {
        val value = expression()
        val memoryIndex = getMemoryIndex.apply()
        currentLongArray(index + (memoryIndex % memorySymbol.slots)) = value
        vcdUpdate(symbol, value)
        if(isVerbose) showIndirectAssignment(symbol, value, memoryIndex)
      }
      else {
        if(isVerbose) {
          println(s"${symbol.name}(${getMemoryIndex.apply() % memorySymbol.slots}) <= NOT ENABLED")
        }
      }
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if(isLean) runLean _ else runFull _
    }
    var run: FuncUnit = if (optimizationLevel == 0) runFull _ else runLean _
  }

  case class AssignBigIndirect(
                                 symbol: Symbol,
                                 memorySymbol: Symbol,
                                 getMemoryIndex: FuncInt,
                                 enable: FuncInt,
                                 expression: FuncBig
                               ) extends Assigner {
    val index: Int = memorySymbol.index

    def runLean(): Unit = {
      if(enable() > 0) {
        currentBigArray(index + (getMemoryIndex.apply() % memorySymbol.slots)) = expression()
      }
    }

    def runFull(): Unit = {
      if(enable() > 0) {
        val value = expression()
        val memoryIndex = getMemoryIndex.apply()
        currentBigArray(index + (memoryIndex % memorySymbol.slots)) = value
        vcdUpdate(symbol, value)
        if(isVerbose) showIndirectAssignment(symbol, value, memoryIndex)
      }
      else {
        if(isVerbose) {
          println(s"${symbol.name}(${getMemoryIndex.apply() % memorySymbol.slots}) <= NOT ENABLED")
        }
      }
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if(isLean) runLean _ else runFull _
    }
    var run: FuncUnit = if (optimizationLevel == 0) runFull _ else runLean _
  }

  case class BlackBoxShim(
      unexpandedName: String,
      outputName:     Symbol,
      inputs:         Seq[Symbol],
      implementation: BlackBoxImplementation
  )
  extends BigExpressionResult {

    val dataStore: DataStore = DataStore.this

    def apply(): Big = {
      val inputValues = inputs.map { input => dataStore(input) }
      val bigInt = implementation.execute(inputValues, outputName.firrtlType, unexpandedName)
      bigInt
    }
  }

  def apply(symbol: Symbol): Big = {
    symbol.dataSize match {
      case IntSize  => currentIntArray(symbol.index)
      case LongSize => currentLongArray(symbol.index)
      case BigSize  => currentBigArray(symbol.index)
    }
  }

  def earlierBufferIndex(lookBack: Int): Int = {
    ((currentBufferIndex + numberOfBuffers) - lookBack) % numberOfBuffers
  }

  def earlierValue(symbol: Symbol, lookBack: Int): Big = {
    val lookBackIndex = earlierBufferIndex(lookBack)

    symbol.dataSize match {
      case IntSize  => intData(lookBackIndex)(symbol.index)
      case LongSize => longData(lookBackIndex)(symbol.index)
      case BigSize  => bigData(lookBackIndex)(symbol.index)
    }
  }

  def update(symbol: Symbol, value: Big): Unit = {
    symbol.dataSize match {
      case IntSize  => currentIntArray(symbol.index) = value.toInt
      case LongSize => currentLongArray(symbol.index) = value.toLong
      case BigSize  => currentBigArray(symbol.index) = value
    }
  }

  def setValueAtIndex(dataSize: DataSize, index: Int, value: Big): Unit = {
    dataSize match {
      case IntSize  => currentIntArray(index)  = value.toInt
      case LongSize => currentLongArray(index) = value.toLong
      case BigSize  => currentBigArray(index)  = value
    }
  }

  def getValueAtIndex(dataSize: DataSize, index: Int): BigInt = {
    dataSize match {
      case IntSize  => currentIntArray(index)
      case LongSize => currentLongArray(index)
      case BigSize  => currentBigArray(index)
    }
  }

  //scalastyle:off cyclomatic.complexity method.length
  def serialize: String = {

    val nextForData = Seq(IntSize, LongSize, BigSize).map { size => size.toString -> nextIndexFor(size) }.toMap

    val intDataRows = intData.zipWithIndex.map { case (row, index) =>
      s"row$index" -> JArray(intData(index).toList.map { a ⇒ val v: JValue = a; v })
    }

    val longDataRows = longData.zipWithIndex.map { case (row, index) =>
      s"row$index" -> JArray(longData(index).toList.map { a ⇒ val v: JValue = a; v })
    }

    val bigDataRows = bigData.zipWithIndex.map { case (row, index) =>
      s"row$index" -> JArray(bigData(index).toList.map { a ⇒ val v: JValue = a; v })
    }

    val intDataPacket = {
      var rowList = intDataRows.head ~ Nil
      for (row <- intDataRows.tail) {
        rowList = rowList ~ row
      }
      rowList
    }

    val longDataPacket = {
      var rowList = longDataRows.head ~ Nil
      for (row <- longDataRows.tail) {
        rowList = rowList ~ row
      }
      rowList
    }

    val bigDataPacket = {
      var rowList = bigDataRows.head ~ Nil
      for (row <- bigDataRows.tail) {
        rowList = rowList ~ row
      }
      rowList
    }

    val json =
      "header" ->
        ("numberOfBuffers" -> numberOfBuffers) ~
          ("currentBufferIndex" -> currentBufferIndex) ~
          ("previousBufferIndex" -> previousBufferIndex) ~
          ("nextForData" -> nextForData) ~
          ("intData" -> intDataPacket) ~
          ("longData" -> longDataPacket) ~
          ("bigData" -> bigDataPacket)

    pretty(render(json))
  }

  def deserialize(jsonString: String): Unit = {

    val json2 = parse(jsonString)

    for {
      JObject(child) <- json2
      JField(fieldName, value) <- child
    } {
      fieldName match {
        case "numberOfBuffers" =>
          val JInt(numBuffs) = value
          if(numBuffs != numberOfBuffers) {
            println(s"WARNING: numberOfBuffers in snapshot $numBuffs does not match runtime $numberOfBuffers")
          }
        case "currentBufferIndex" =>
          val JInt(index) = value
          currentBufferIndex = index.toInt
          // println(s"setting currentBufferIndex to $currentBufferIndex")
        case "previousBufferIndex" =>
          val JInt(index) = value
          previousBufferIndex = index.toInt
          // println(s"setting previousBufferIndex to $previousBufferIndex")
        case "nextForData" =>
          for {
            JObject(child2) <- value
            JField(fieldName, value) <- child2
          } {
            val JInt(nextNumber) = value
            fieldName match {
              case "Int"  => nextIndexFor(IntSize)  = nextNumber.toInt
              case "Long" => nextIndexFor(LongSize) = nextNumber.toInt
              case "Big"  => nextIndexFor(BigSize)  = nextNumber.toInt
            }
          }
        case "intData" =>
          for {
            JObject(child2) <- value
            JField(arrayName, JArray(intValues)) <- child2
          } {
            val arrayNumber = arrayName.drop("row".length).toInt
            intValues.zipWithIndex.foreach {
              case (JInt(v), index) => intData(arrayNumber)(index) = v.toInt
              case _ => None
            }
          }
        case "longData" =>
          for {
            JObject(child2) <- value
            JField(arrayName, JArray(intValues)) <- child2
          } {
            val arrayNumber = arrayName.drop("row".length).toInt
            intValues.zipWithIndex.foreach {
              case (JInt(v), index) => longData(arrayNumber)(index) = v.toLong
              case _ => None
            }
          }
        case "bigData" =>
          for {
            JObject(child2) <- value
            JField(arrayName, JArray(intValues)) <- child2
          } {
            val arrayNumber = arrayName.drop("row".length).toInt
            intValues.zipWithIndex.foreach {
              case (JInt(v), index) => bigData(arrayNumber)(index) = v
              case _ => None
            }
          }
        case _ =>
          // println(s"$fieldName -> $value")
      }
    }
  }
}

object DataStore {
  def apply(numberOfBuffers: Int, optimizationLevel: Int): DataStore = {
    new DataStore(numberOfBuffers, optimizationLevel)
  }
}
