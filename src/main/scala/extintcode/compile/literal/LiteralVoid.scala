package extintcode.compile.literal

import extintcode.asm.{AssemblyData, AssemblyText, Direct, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}

object LiteralVoid extends LangExpression {
  
  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = (Nil, Nil)

  override def pointer(): Boolean = false
  override def result(result: CompilerRuntime): ValType = Direct(0, null)
}
