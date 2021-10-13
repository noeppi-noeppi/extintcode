package extintcode.compile.postprocess

import extintcode.asm._
import extintcode.compile.frame.{FrameWalker, ReturnFrame}
import extintcode.util.IntCodeRuntime

// Simplify a call followed by a jmp
object TailCallProcessor extends PostProcessor {
  
  override def process(text: List[AssemblyText], data: List[AssemblyData]): (List[AssemblyText], List[AssemblyData]) = {
    val walker = new Walker(text)
    (walker.walk(), data)
  }
  
  private class Walker(text: List[AssemblyText]) extends FrameWalker(text) {
    
    override def process(stmt: AssemblyText): Seq[AssemblyText] = stmt match {
      case StmtPush(amount) =>
        peek(1) match {
          case Some(StmtCall(memory)) =>
            peek(2) match {
              case Some(StmtPop(amount2)) if amount == amount2 =>
                peek(3) match {
                  case Some(StmtJmp(target)) if frames(3).exists(f => f.isInstanceOf[ReturnFrame]) =>
                    // push and pop can be dropped because we'll leave stack section
                    // instantly as we have a return frame
                    consume(3)
                    List(
                      StmtMov(target, SpecialValue(IntCodeRuntime.Names.BACKJUMP)),
                      StmtJmp(memory)
                    )
                  case _ => List(stmt)
                }
              case _ => List(stmt)
            }
          case _ => List(stmt)
        }
      case stmt => List(stmt)
    }
  }
}
