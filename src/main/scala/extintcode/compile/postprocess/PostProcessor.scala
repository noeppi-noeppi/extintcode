package extintcode.compile.postprocess

import extintcode.asm.{AssemblyData, AssemblyText}

trait PostProcessor {
  
  def process(text: List[AssemblyText], data: List[AssemblyData]): (List[AssemblyText], List[AssemblyData])
}

object PostProcessor {
  
  private val PROCESSORS: List[PostProcessor] = List(
    TrivialPointers,
    MovCollapse
  )
  
  def processors(enable: Boolean, frames: Boolean): List[PostProcessor] = {
    if (enable && frames) {
      PROCESSORS
    } else if (enable) {
      PROCESSORS ::: RemoveFrames :: Nil
    } else if (frames) {
      Nil
    } else {
      RemoveFrames :: Nil
    }
  }
}