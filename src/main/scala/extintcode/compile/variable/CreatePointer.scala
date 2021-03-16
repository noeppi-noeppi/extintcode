package extintcode.compile.variable

import extintcode.asm.{AssemblyData, AssemblyText, Direct, StmtDyn, StmtStore, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}

class CreatePointer(value: LangExpression) extends LangExpression {
  
  private var v: ValType = _
  
  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val (code, data) = value.code(imports, runtime)
    if (value.pointer()) throw new IllegalStateException("Can't create pointer from pointer. Use a type cast to suppress.")
    v = runtime.createExpressionResult()
    (code.appendedAll(List(
      StmtDyn(Direct(1, null), v),
      StmtStore(value.value(runtime), v)
    )), data)
  }
  
  override def pointer(): Boolean = true
  override protected def result(runtime: CompilerRuntime): ValType = v
}
