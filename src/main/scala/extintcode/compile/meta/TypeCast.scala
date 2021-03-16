package extintcode.compile.meta

import extintcode.asm.{AssemblyData, AssemblyText, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}

class TypeCast(value: LangExpression, mode: Boolean) extends LangExpression {
  
  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val result = value.code(imports, runtime)
    if (value.pointer() == mode) {
      println("Warning: Unnecessary type cast: " + value.getClass.getSimpleName)
    }
    result
  }
  
  override def pointer(): Boolean = mode
  override protected def result(runtime: CompilerRuntime): ValType = value.value(runtime)

  override def constantExpression(runtime: CompilerRuntime): Option[Long] = None
}
