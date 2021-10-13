package extintcode.compile.postprocess
import extintcode.asm.{AssemblyData, AssemblyText, SpecialValue, StmtCall, StmtJmp, StmtMov}
import extintcode.compile.frame.FrameWalker
import extintcode.util.IntCodeRuntime

// Simplify a call followed by a jmp
object CallJumpProcessor extends PostProcessor {
  
  override def process(text: List[AssemblyText], data: List[AssemblyData]): (List[AssemblyText], List[AssemblyData]) = {
    val walker = new Walker(text)
    (walker.walk(), data)
  }
  
  private class Walker(text: List[AssemblyText]) extends FrameWalker(text) {
    
    override def process(stmt: AssemblyText): Seq[AssemblyText] = stmt match {
      case StmtCall(memory) =>
        peek() match {
          case Some(StmtJmp(target)) =>
            consume()
            // Move target address into backjump immediately
            // and then jump to call target instead of using call
            // which assembles to two IntCode instructions
            List(
              StmtMov(target, SpecialValue(IntCodeRuntime.Names.BACKJUMP)),
              StmtJmp(memory)
            )
          case _ => List(stmt)
        }
      case stmt => List(stmt)
    }
  }
}
