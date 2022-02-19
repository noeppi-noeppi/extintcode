package extintcode.compile.postprocess
import extintcode.asm.{AssemblyData, AssemblyText, Direct, DirectLabel, SpecialValue, StmtCall, StmtJmp, StmtMov, ValType}
import extintcode.compile.frame.FrameWalker
import extintcode.util.IntCodeRuntime

// Simplify a call followed by a jmp
object CallJumpProcessor extends PostProcessor {
  
  override def process(text: List[AssemblyText], data: List[AssemblyData]): (List[AssemblyText], List[AssemblyData]) = {
    val walker = new Walker(text, constantTarget = false)
    val constWalker = new Walker(walker.walk(), constantTarget = true)
    (constWalker.walk(), data)
  }

  // For constant jump targets we can peek through end frames
  private class Walker(text: List[AssemblyText], constantTarget: Boolean) extends FrameWalker(text, peekThroughEndFrames = constantTarget) {
    
    override def process(stmt: AssemblyText): Seq[AssemblyText] = stmt match {
      case StmtCall(memory) =>
        peek() match {
          case Some(StmtJmp(target)) if !constantTarget || checkConst(target) =>
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
    
    private def checkConst(target: ValType): Boolean = target match {
      case Direct(_, null) => true
      case Direct(_, "") => true
      case DirectLabel(_) => true
      case _ => false
    }
  }
}
