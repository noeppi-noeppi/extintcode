package extintcode.compile.frame

import extintcode.asm.ValType
import extintcode.util.FunctionEntry

case class GlobalFrame(name: String, address: ValType) extends Frame {
  override def comment(): String = "global " + name + " " + address.string()
}

case class VariableFrame(name: String, address: ValType) extends Frame {
  override def comment(): String = "variable " + name + " " + address.string()
}

case class CallFrame(module: String, func: FunctionEntry) extends Frame {
  override def comment(): String = "call " + module + " " + func
}

case class ScopeFrame(override val at: Int) extends StartFrame(at) {
  override def comment(): String = "begin scope " + at
}

case class StackFrame(override val at: Int) extends StartFrame(at) {
  override def comment(): String = "begin stack " + at
}

case class ExpressionFrame(override val at: Int) extends StartFrame(at) {
  override def comment(): String = "begin expression " + at
}

