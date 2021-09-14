package extintcode.compile.postprocess
import extintcode.asm.{AssemblyData, AssemblyText}
import extintcode.compile.frame.Frame

object RemoveFrames extends PostProcessor {
  
  override def process(text: List[AssemblyText], data: List[AssemblyData]): (List[AssemblyText], List[AssemblyData]) = {
    (text.filter(!_.isInstanceOf[Frame]), data.filter(!_.isInstanceOf[Frame]))
  }
}
