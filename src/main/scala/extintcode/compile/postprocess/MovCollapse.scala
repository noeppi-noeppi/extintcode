package extintcode.compile.postprocess
import extintcode.asm.{AssemblyData, AssemblyText, MemoryStack, StmtAdd, StmtEq, StmtInp, StmtLoad, StmtLt, StmtMov, StmtMul, StmtStore}
import extintcode.compile.frame.FrameWalker

// Collapse two mov instructions into one if the first one
// stores to the stack, the second one reads from the stack 
// and the same stack address is never used again in it's matching
// frame
object MovCollapse extends PostProcessor {
  
  override def process(text: List[AssemblyText], data: List[AssemblyData]): (List[AssemblyText], List[AssemblyData]) = {
    val walker = new Walker(text)
    (walker.walk(), data)
  }
  
  private class Walker(text: List[AssemblyText]) extends FrameWalker(text, peekInExpressions = true) {
    override def process(stmt: AssemblyText): Seq[AssemblyText] = stmt match {
      case StmtMov(from1, to1 @ MemoryStack(_, null)) if expression() =>
        peek() match {
          case Some(StmtMov(from2, to2)) if to1 == from2 && !findMemory(1, to1) =>
            consume()
            List(StmtMov(from1, to2))
          case Some(StmtStore(from2, to2)) if to1 == from2 && !findMemory(1, to1) =>
            consume()
            List(StmtStore(from1, to2))
          case _ => List(stmt)
        }
      case StmtLoad(from1, to1 @ MemoryStack(_, null)) if expression() =>
        peek() match {
          case Some(StmtMov(from2, to2)) if to1 == from2 && !findMemory(1, to1) =>
            consume()
            List(StmtLoad(from1, to2))
          case _ => List(stmt)
        }
      case StmtAdd(op1, op2, to1 @ MemoryStack(_, null)) if expression() =>
        peek() match {
          case Some(StmtMov(from2, to2)) if to1 == from2 && !findMemory(1, to1) =>
            consume()
            List(StmtAdd(op1, op2, to2))
          case _ => List(stmt)
        }
      case StmtMul(op1, op2, to1 @ MemoryStack(_, null)) if expression() =>
        peek() match {
          case Some(StmtMov(from2, to2)) if to1 == from2 && !findMemory(1, to1) =>
            consume()
            List(StmtMul(op1, op2, to2))
          case _ => List(stmt)
        }
      case StmtInp(to1 @ MemoryStack(_, null)) if expression() =>
        peek() match {
          case Some(StmtMov(from2, to2)) if to1 == from2 && !findMemory(1, to1) =>
            consume()
            List(StmtInp(to2))
          case _ => List(stmt)
        }
      case StmtLt(op1, op2, to1 @ MemoryStack(_, null)) if expression() =>
        peek() match {
          case Some(StmtMov(from2, to2)) if to1 == from2 && !findMemory(1, to1) =>
            consume()
            List(StmtLt(op1, op2, to2))
          case _ => List(stmt)
        }
      case StmtEq(op1, op2, to1 @ MemoryStack(_, null)) if expression() =>
        peek() match {
          case Some(StmtMov(from2, to2)) if to1 == from2 && !findMemory(1, to1) =>
            consume()
            List(StmtEq(op1, op2, to2))
          case _ => List(stmt)
        }
      case _ => List(stmt)
    }
  }
}
