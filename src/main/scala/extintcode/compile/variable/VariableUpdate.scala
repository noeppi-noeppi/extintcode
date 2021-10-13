package extintcode.compile.variable

import extintcode.asm.{AssemblyData, AssemblyText, StmtMov}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression, LangStatement}

import scala.collection.mutable.ListBuffer

class VariableUpdate(name: String, value: LangExpression) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val variable = runtime.getVariable(name)
    variable.redef()
    variable.noConst()
    val text = ListBuffer[AssemblyText]()
    text.addAll(runtime.startExpressionSection())
    val (t, data) = value.code(imports, runtime)
    text.addAll(t)
    runtime.checkType("variable update", variable.pointer, value.pointer())
    text.addOne(StmtMov(value.value(runtime), variable.location))
    text.addAll(runtime.endExpressionSection())
    (text.toList, data)
  }
}
