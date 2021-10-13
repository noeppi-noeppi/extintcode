package extintcode.compile.frame

import extintcode.asm.{AssemblyData, AssemblyText}

abstract class Frame extends AnyRef with AssemblyText with AssemblyData {
  
  def comment(): String
  
  override final def textStr(): String = "    ; " + comment()
  override final def dataStr(): String = "; " + comment()
}

// Marker for FrameWalker that a frame is bound to the statement immediately
// following it.
abstract class StmtFrame extends Frame

// Frame needs a matching EndFrame
// `at` is the start position of the frame relative to the current stack
// Increases current stack by that value
abstract class StartFrame(val at: Int) extends Frame

case object TopLevelFrame extends StartFrame(0) {
  override def comment(): String = "; toplevel"
}

case object EndFrame extends Frame {
  override def comment(): String = "end"
}