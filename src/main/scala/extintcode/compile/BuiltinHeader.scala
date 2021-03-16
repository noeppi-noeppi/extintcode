package extintcode.compile

import extintcode.asm.{AssemblyText, Direct, SpecialValue, StmtInp, StmtOutp, StmtRet, ValType}
import extintcode.util.{FunctionEntry, HeaderModule, IntCodeRuntime}

object BuiltinHeader extends HeaderModule(IntCodeRuntime.Modules.BUILTIN, 0, Nil, List(
  (FunctionEntry("ichr", pure = true, pointerReturn = false, List()), -1),
  (FunctionEntry("ochr", pure = true, pointerReturn = false, List(false)), -1),
  (FunctionEntry("exit", pure = true, pointerReturn = false, List()), -1)
)) {
  
  def specialFunctionCode(func: (FunctionEntry, ValType), args: ValType*): Option[List[AssemblyText]] = {
    val (function, memory) = func
    memory match {
      case Direct(_, this.name) =>
      case _ => return None
    }
    function.name match {
      case "ichr" if function.signature.isEmpty => Some(List(
        StmtInp(SpecialValue(IntCodeRuntime.Names.RETURN))
      ))
      case "ochr" if function.signature.size == 1 => Some(List(
        StmtOutp(args(0))
      ))
      case "exit" if function.signature.isEmpty => Some(List(
        StmtRet
      ))
      case _ => throw new IllegalStateException("Unknown builtin compiler function: " + function)
    }
  }
}
