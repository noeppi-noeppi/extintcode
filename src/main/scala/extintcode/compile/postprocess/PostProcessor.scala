package extintcode.compile.postprocess

import extintcode.asm.{AssemblyData, AssemblyText}

trait PostProcessor {
  
  def process(text: List[AssemblyText], data: List[AssemblyData]): (List[AssemblyText], List[AssemblyData])
}

object PostProcessor {
  
  // Contains entries multiple times because
  // they need to be applied again after another
  // processor was applied.
  private val PROCESSORS: List[PostProcessor] = List(
    TrivialPointers,
    TrivialMov,
    SimpleJumps,
    MovCollapse,
    TrivialMov,
    CallJumpProcessor,
    TailCallProcessor,
    TrivialMov,
    MovCollapse,
    SimpleJumps,
    TrivialMov
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