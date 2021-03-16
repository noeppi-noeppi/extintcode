package extintcode.compile.function

import extintcode.asm.{AssemblyData, AssemblyText, SpecialValue, StmtJmp, StmtMov}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression, LangStatement}
import extintcode.util.IntCodeRuntime

class ReturnStatement(value: LangExpression) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    if (runtime.stackSectionResultType.isEmpty) throw new IllegalStateException("Return statements can only be used inside a function.")
    runtime.startExpressionSection()
    val (code, data) = value.code(imports, runtime)
    runtime.endExpressionSection()
    runtime.checkType("value to return", runtime.stackSectionResultType.get, value.pointer())
    val backjump = runtime.getVariable("~backjump")
    (code.appendedAll(List(
      StmtMov(value.value(runtime), SpecialValue(IntCodeRuntime.Names.RETURN)),
      StmtJmp(backjump.location)
    )), data)
  }
}
