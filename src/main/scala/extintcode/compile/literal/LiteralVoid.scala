package extintcode.compile.literal

import extintcode.asm.{AssemblyData, AssemblyText, Direct, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}

// Void literal behaves almost like a 0 but has some special handling
// For example returning void fro ma function won't set the RETURN value
// keeping its old state that is unknown.
object LiteralVoid extends LangExpression {
  
  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = (Nil, Nil)

  override def pointer(): Boolean = false
  override def result(result: CompilerRuntime): ValType = Direct(0, null)
}
