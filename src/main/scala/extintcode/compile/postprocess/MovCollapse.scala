package extintcode.compile.postprocess
import extintcode.asm.{AssemblyData, AssemblyText, MemoryStack, StmtMov}
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
  
  private class Walker(text: List[AssemblyText]) extends FrameWalker(text) {
    override def process(stmt: AssemblyText): AssemblyText = stmt match {
      case StmtMov(from1, to1 @ MemoryStack(_, null)) if expression() =>
        peek() match {
          case Some(StmtMov(from2, to2)) if to1 == from2 && !findMemory(1, to1) =>
            consume()
            StmtMov(from1, to2)
          case _ => stmt
        }
      case _ => stmt
    }
  }
}
