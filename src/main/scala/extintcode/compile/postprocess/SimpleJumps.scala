package extintcode.compile.postprocess
import extintcode.asm.{AssemblyData, AssemblyText, StmtJmp, StmtJnz, StmtJz, StmtLoad, StmtMov, StmtStore}

// Remove jumps that never jump and turn conditional jumps that always jump
// into jmp instructions to be processed by other processors easier.
object SimpleJumps extends PostProcessor {
  
  override def process(text: List[AssemblyText], data: List[AssemblyData]): (List[AssemblyText], List[AssemblyData]) = {
    val replaced = text.flatMap {
      case StmtJnz(in, _) if in.directValue().contains(0) => None
      case StmtJnz(in, target) if in.directValue().exists(_!= 0) => Some(StmtJmp(target))
      case StmtJz(in, _) if in.directValue().exists(_ != 0) => None
      case StmtJz(in, target) if in.directValue().contains(0) => Some(StmtJmp(target))
      case stmt => Some(stmt)
    }
    (replaced, data)
  }
}
