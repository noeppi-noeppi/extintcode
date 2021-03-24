package extintcode.compile.variable

import extintcode.asm.{AssemblyData, AssemblyText, StmtMov}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression, LangStatement}

class VariableUpdate(name: String, value: LangExpression) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val variable = runtime.getVariable(name)
    variable.redef()
    variable.noConst()
    runtime.startExpressionSection()
    val (code, data) = value.code(imports, runtime)
    runtime.endExpressionSection()
    runtime.checkType("variable update", variable.pointer, value.pointer())
    (code.appended(StmtMov(value.value(runtime), variable.location)), data)
  }
}
