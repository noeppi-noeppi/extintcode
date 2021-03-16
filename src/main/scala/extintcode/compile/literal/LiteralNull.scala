package extintcode.compile.literal

import extintcode.asm.{AssemblyData, AssemblyText, Direct, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}

object LiteralNull extends LangExpression {
  
  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = (Nil, Nil)

  override def pointer(): Boolean = true
  override def result(result: CompilerRuntime): ValType = Direct(-1, null)
  def plainResult: Long = -1
}
