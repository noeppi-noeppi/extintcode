package extintcode.compile.frame

import extintcode.asm.{AssemblerLabel, AssemblerStatement, AssemblyText, MemoryStack, StmtRaw, ValType}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

abstract class FrameWalker(val text: List[AssemblyText]) {
  
  private var idx = 0
  
  private val frameStack = mutable.Stack[FrameEntry](
    new FrameEntry(TopLevelFrame)
  )
  
  private var peeking = false
  
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
          processFrame(frame, canModifyStack = true)
          builder.addOne(frame)
          idx += 1
        case oldStmt =>
          val newStmt = process(oldStmt)
          builder.addOne(newStmt)
          endPeek()
          idx += 1
      }
    }
    endWalk()
    builder.toList
  }
  
  private def processFrame(frame: Frame, canModifyStack: Boolean): Unit = frame match {
    case TopLevelFrame => throw new IllegalStateException("Internal compiler error: Invalid frames: TopLevel Frame: " + idx)
    case f: StackFrame if canModifyStack => stackSections += 1; frameStack.push(new FrameEntry(f))
    case f: ExpressionFrame if canModifyStack => expressionSections += 1; frameStack.push(new FrameEntry(f))
    case f: StartFrame if canModifyStack => frameStack.push(new FrameEntry(f))
    case EndFrame if canModifyStack && frameStack.size <= 1 => throw new IllegalStateException("Internal compiler error: Invalid frames: Closing unopened start frame: " + idx)
    case EndFrame if canModifyStack && frameStack.head.frame.isInstanceOf[StackFrame] => stackSections -= 1; frameStack.pop()
    case EndFrame if canModifyStack && frameStack.head.frame.isInstanceOf[ExpressionFrame] => expressionSections -= 1; frameStack.pop()
    case EndFrame if canModifyStack=> frameStack.pop()
    case f: GlobalFrame => frameStack.head.addGlobal(f)
    case f: VariableFrame => frameStack.head.addVariable(f)
    case _: CallFrame =>
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
    case _: StartFrame => false
    case EndFrame => false
    case _ => true
  }
  
  
  def process(stmt: AssemblyText): AssemblyText
  
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
      text(idx + peekedIdx) match {
        case frame: Frame => processFrame(frame, canModifyStack = false); peekedIdx += 1
        case _: AssemblerLabel if !labels => peekedIdx += 1
        case stmt =>
          if (current >= n) { 
            return Some(stmt)
          } else {
            current += 1
            peekedIdx += 1
          }
      }
    }
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
  }
  
  def consume(): Unit = consume(1)
  def consume(n: Int): Unit = consume(n, labels = true)
  def consume(n: Int, labels: Boolean): Unit = {
    var dropsLeft = n
    while (dropsLeft > 0) {
      if (idx + 1 >= text.size || !canPeek(text(idx + 1))) throw new IllegalStateException("Can't drop " + n + " statements: End of frame reached: " + idx)
      text(idx + 1) match {
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
    var current = idx + 1 + skip
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
  
  private class FrameEntry(val frame: StartFrame) {
    
    private[this] val globalList = ListBuffer[GlobalFrame]()
    private[this] val variableList = ListBuffer[VariableFrame]()
    
    def addGlobal(frame: GlobalFrame): Unit = globalList.addOne(frame)
    def addVariable(frame: VariableFrame): Unit = variableList.addOne(frame)
    
    def globals: List[GlobalFrame] = globalList.toList
    def variables: List[VariableFrame] = variableList.toList
  }
}
