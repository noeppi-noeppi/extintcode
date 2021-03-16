package extintcode.compile.variable

import extintcode.asm.{AssemblyData, AssemblyText, StmtLoad, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}

class VariableDeref(name: String) extends LangExpression {
  
  private var v: ValType = _
  
  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val variable = runtime.getVariable(name)
    if (!variable.pointer) throw new IllegalStateException("Can't dereference direct variable: " + name + " Use a cast to suppress.")
    v = runtime.createExpressionResult()
    (List(StmtLoad(variable.location, v)), Nil)
  }

  override def pointer(): Boolean = false
  override protected def result(result: CompilerRuntime): ValType = v
}
