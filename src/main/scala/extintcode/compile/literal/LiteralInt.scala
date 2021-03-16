package extintcode.compile.literal

import extintcode.asm.{AssemblyData, AssemblyText, Direct, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}

class LiteralInt(value: Long) extends LangExpression {
  
  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = (Nil, Nil)

  override def pointer(): Boolean = false
  override def result(result: CompilerRuntime): ValType = Direct(value, null)

  override def constantExpression(runtime: CompilerRuntime): Option[Long] = Some(value)
}
