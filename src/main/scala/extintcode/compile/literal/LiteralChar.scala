package extintcode.compile.literal

import extintcode.asm.IntCodeAssembler

class LiteralChar(char: String) extends LiteralInt({
  val codePoints = IntCodeAssembler.stringToCodePoints(char)
  if (codePoints.size != 1) throw new IllegalStateException("Invalid character literal: '" + char + "'.")
  codePoints.head
})
