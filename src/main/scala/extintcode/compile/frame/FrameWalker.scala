package extintcode.compile.frame

import extintcode.asm.{AssemblerLabel, AssemblerStatement, AssemblyText, MemoryStack, StmtRaw, ValType}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

// Careful with peekInExpressions. It will peek into expressions but don't give a sign that it did.
// expression() will still be false. Just like if there has not been an expression at all
// Should be false in most cases.
// Same goes for peekThroughEndFrames. Also end frames won't be consumed by the consume function and
// will remain in place AFTER the newly generated statements.
abstract class FrameWalker(val text: List[AssemblyText], val peekInExpressions: Boolean = false, val peekThroughEndFrames: Boolean = false) {
  
  private var idx = 0
  
  private val frameStack = mutable.Stack[FrameEntry](
    new FrameEntry(TopLevelFrame)
  )
  
  // For each peeked statement.
  private val currentStmtFrames: ListBuffer[ListBuffer[StmtFrame]] = ListBuffer()
  
  private var peeking = false
  private var totalPeeked = 0
  
  // Start and end frames can't be consumed
  private var consumedNestableFrames: ListBuffer[Frame] = ListBuffer()
  
  // Compiler does not use nested stack or expression sections
  // but frames should allow this
  private var stackSections = 0
  private var expressionSections = 0
  
  def walk(): List[AssemblyText] = {
    val builder = ListBuffer[AssemblyText]()
    // Idx may be changed during the loop
    while (idx < text.size) {
      text(idx) match {
        case frame: Frame =>
          processFrame(frame, canModifyStack = true, currentPeekedIdx = 0)
          builder.addOne(frame)
          idx += 1
        case oldStmt =>
          val newStmts = process(oldStmt)
          builder.addAll(newStmts)
          consumedNestableFrames.foreach(frame => processFrame(frame, canModifyStack = true, currentPeekedIdx = 0))
          builder.addAll(consumedNestableFrames)
          consumedNestableFrames.clear()
          endPeek()
          idx += 1
          currentStmtFrames.clear()
          currentStmtFrames.addOne(ListBuffer())
      }
    }
    endWalk()
    builder.toList
  }
  
  private def processFrame(frame: Frame, canModifyStack: Boolean, currentPeekedIdx: Int): Unit = frame match {
    case TopLevelFrame => throw new IllegalStateException("Internal compiler error: Invalid frames: TopLevel Frame: " + idx)
    case f: StackFrame if canModifyStack => stackSections += 1; frameStack.push(new FrameEntry(f))
    case f: ExpressionFrame if canModifyStack => expressionSections += 1; frameStack.push(new FrameEntry(f))
    case _: ExpressionFrame if peekInExpressions => // Do nothing.
    case f: StartFrame if canModifyStack => frameStack.push(new FrameEntry(f))
    case EndFrame if canModifyStack && frameStack.size <= 1 => throw new IllegalStateException("Internal compiler error: Invalid frames: Closing unopened start frame: " + idx)
    case EndFrame if canModifyStack && frameStack.head.frame.isInstanceOf[StackFrame] => stackSections -= 1; frameStack.pop()
    case EndFrame if canModifyStack && frameStack.head.frame.isInstanceOf[ExpressionFrame] => expressionSections -= 1; frameStack.pop()
    case EndFrame if canModifyStack => frameStack.pop()
    case EndFrame if peekThroughEndFrames => // Do nothing.
    case f: GlobalFrame => frameStack.head.addGlobal(f)
    case f: VariableFrame => frameStack.head.addVariable(f)
    case f: StmtFrame => currentStmtFrames(currentPeekedIdx).addOne(f)
    case f => println("Warning: Processing unknown frame: " + f)
  }
  
  private def endWalk(): Unit = {
    if (this.frameStack.size != 1) {
      throw new IllegalStateException("Internal compiler error: Invalid frames: Unclosed frames")
    } else if (this.frameStack.head.frame != TopLevelFrame) {
      throw new IllegalStateException("Internal compiler error: Invalid frames: Not TopLevel frame after processing")
    }
  }
  
  private def canPeek(stmt: AssemblyText): Boolean = stmt match {
    case _: ExpressionFrame => peekInExpressions
    case _: StartFrame => false
    case EndFrame => peekThroughEndFrames
    case _ => true
  }
  
  def process(stmt: AssemblyText): Seq[AssemblyText]
  
  def peek(): Option[AssemblyText] = peek(1)
  def peek(n: Int): Option[AssemblyText] = peek(n, labels = true)
  def peek(n: Int, labels: Boolean): Option[AssemblyText] = {
    if (n == 0) return Some(text(idx))
    var current = 1
    var peekedIdx = 1
    while (current <= n) {
      if (idx + peekedIdx >= text.size || !canPeek(text(idx + peekedIdx))) return None
      if (!peeking) {
        peeking = true
        frameStack.push(new FrameEntry(TopLevelFrame))
      }
      while (currentStmtFrames.size <= peekedIdx) {
        currentStmtFrames.addOne(ListBuffer())
      }
      text(idx + peekedIdx) match {
        case frame: Frame if peekedIdx > totalPeeked =>
          processFrame(frame, canModifyStack = false, current)
          peekedIdx += 1
        case _: Frame =>
          peekedIdx += 1
        case _: AssemblerLabel if !labels =>
          peekedIdx += 1
        case stmt =>
          if (current >= n) {
            totalPeeked = peekedIdx
            return Some(stmt)
          } else {
            current += 1
            peekedIdx += 1
          }
      }
    }
    totalPeeked = peekedIdx
    None
  }
  
  private def endPeek(): Unit = {
    if (peeking) {
      peeking = false
      // Pop synthetic entry for peeked frames
      // Always only one as it's impossible to peek
      // into start frames
      frameStack.pop()
    }
    totalPeeked = 0
  }
  
  def consume(): Unit = consume(1)
  def consume(n: Int): Unit = consume(n, labels = true)
  def consume(n: Int, labels: Boolean): Unit = {
    var dropsLeft = n
    while (dropsLeft > 0) {
      if (idx + 1 >= text.size || !canPeek(text(idx + 1))) throw new IllegalStateException("Can't drop " + n + " statements: End of frame reached: " + idx)
      text(idx + 1) match {
        case f: StartFrame => consumedNestableFrames.addOne(f); idx += 1
        case EndFrame => consumedNestableFrames.addOne(EndFrame); idx += 1
        case _: Frame => idx += 1
        case _: AssemblerLabel if !labels => idx += 1
        case _ => idx += 1; dropsLeft -= 1
      }
    }
  }
  
  def findType[T <: AssemblyText : ClassTag](): Boolean = findType[T](0)
  def findType[T <: AssemblyText : ClassTag](skip: Int): Boolean = find(skip, {
    case _: T => true
    case _ => false
  })
  
  def findMemory(memory: ValType): Boolean = findMemory(0, memory)
  def findMemory(skip: Int, memory: ValType): Boolean = memory match {
    case MemoryStack(stackIdx, null) => findStack(stackIdx, skip) // Search to end of valid stack area
    case MemoryStack(_, _) => true // Can't handle relocated stack memory
    case mem => find(skip, {
      case _: StmtRaw => true
      case stmt: AssemblerStatement => stmt.values().contains(mem)
      case _ => false
    })
  }
  
  def find(predicate: AssemblyText => Boolean): Boolean = find(0, predicate)
  def find(skip: Int, predicate: AssemblyText => Boolean): Boolean = find(0, skip, predicate)
  
  private def findFramesUp(stackIdx: Long): Int = {
    var usedVars = 0
    for (entry <- frameStack.indices.reverse) {
      usedVars += frameStack(entry).frame.at
      if (stackIdx <= usedVars) {
        return entry + 1
      }
    }
    0
  }
  
  private def findStack(stackIdx: Long, skip: Int): Boolean = find(findFramesUp(stackIdx), skip, {
    case _: StmtRaw => true
    case stmt: AssemblerStatement => stmt.values().contains(MemoryStack(stackIdx, null))
    case _ => false
  })
  
  def find(framesUp: Int, skip: Int, predicate: AssemblyText => Boolean): Boolean = {
    var endFramesLeft = framesUp
    var current = idx + 1
    var skipsLeft = skip
    while (skipsLeft > 0) {
      current += 1
      if (current >= text.size || !text(current).isInstanceOf[Frame]) skipsLeft -= 1
    }
    while (endFramesLeft >= 0 && current < text.size) {
      text(current) match {
        case EndFrame => endFramesLeft -= 1
        case _: Frame =>
        case stmt if predicate(stmt) => return true
        case _ =>
      }
      current += 1
    }
    false
  }
  
  def stack(): Boolean = stackSections > 0
  def expression(): Boolean = expressionSections > 0
  
  def store(address: ValType): Option[Frame] = variable(address).orElse(global(address))
  def variable(address: ValType): Option[VariableFrame] = frameStack.flatMap(e => e.variables.find(f => f.address == address)).headOption
  def global(address: ValType): Option[GlobalFrame] = frameStack.flatMap(e => e.globals.find(f => f.address == address)).headOption
  def frames(): List[StmtFrame] = frames(0)
  def frames(peekIdx: Int): List[StmtFrame] = {
    if (peekIdx < 0) throw new IllegalArgumentException("PostProcessor error: Can't retrieve past statement frames")
    if (peekIdx >= currentStmtFrames.size) throw new IllegalArgumentException("PostProcessor error: Can't retrieve statement frames: Not peeked far enough: " + peekIdx)
    currentStmtFrames(peekIdx).toList
  }
  
  private class FrameEntry(val frame: StartFrame) {
    
    private[this] val globalList = ListBuffer[GlobalFrame]()
    private[this] val variableList = ListBuffer[VariableFrame]()
    
    def addGlobal(frame: GlobalFrame): Unit = globalList.addOne(frame)
    def addVariable(frame: VariableFrame): Unit = variableList.addOne(frame)
    
    def globals: List[GlobalFrame] = globalList.toList
    def variables: List[VariableFrame] = variableList.toList
  }
}
