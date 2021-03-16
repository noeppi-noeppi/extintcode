package extintcode.compile.variable

import extintcode.asm.{AssemblyData, AssemblyText, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}

class VariableAccess(name: String) extends LangExpression {
  
  private var v: ValType = _
  private var p: Boolean = _
  
  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val variable = runtime.getVariable(name)
    v = variable.location
    p = variable.pointer
    (Nil, Nil)
  }

  override def pointer(): Boolean = p
  override protected def result(result: CompilerRuntime): ValType = v
}
