package extintcode.compile.postprocess
import extintcode.asm._

// Turn load and store instructions into mov instructions if they use
// a pointer in direct mode
object TrivialPointers extends PostProcessor {
  
  override def process(text: List[AssemblyText], data: List[AssemblyData]): (List[AssemblyText], List[AssemblyData]) = {
    val replaced = text.map {
      case StmtLoad(pointer, out) if pointer.immediatePointer().isDefined => StmtMov(pointer.immediatePointer().get, out)
      case StmtStore(in, pointer) if pointer.immediatePointer().isDefined => StmtMov(in, pointer.immediatePointer().get)
      case stmt => stmt
    }
    (replaced, data)
  }
}
