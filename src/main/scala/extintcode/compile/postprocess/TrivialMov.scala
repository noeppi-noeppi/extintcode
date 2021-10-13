package extintcode.compile.postprocess
import extintcode.asm.{AssemblyData, AssemblyText, StmtMov}

// Remove mov that moves to itself
object TrivialMov extends PostProcessor {
  
  override def process(text: List[AssemblyText], data: List[AssemblyData]): (List[AssemblyText], List[AssemblyData]) = {
    val replaced = text.flatMap {
      case StmtMov(from, to) if from == to => None
      case stmt => Some(stmt)
    }
    (replaced, data)
  }
}
