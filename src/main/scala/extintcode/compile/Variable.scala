package extintcode.compile

import extintcode.asm.ValType

class Variable(val name: String, val location: ValType, val pointer: Boolean, val const: Boolean) {
  
  private var possiblyConst = !pointer
  
  def noConst(): Unit = possiblyConst = false
  def redef(): Unit = if (const) throw new IllegalStateException("Reassignment to const: " + name)
  def canBeConst: Boolean = possiblyConst
}
