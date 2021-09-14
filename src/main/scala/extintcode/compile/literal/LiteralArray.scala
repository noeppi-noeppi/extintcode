package extintcode.compile.literal

import extintcode.asm.{AssemblyData, AssemblyText, DataByValue, DataIntArray, DirectData, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}

class LiteralArray(val values: List[Long]) extends LangExpression {
  
  private var v: ValType = _
  
  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val name = runtime.newDataEntry("literal")
    val data = List(
      DataByValue(name, DataIntArray(values.map((_, null)): _*))
    )
    v = DirectData(name)
    (Nil, data)
  }

  override def pointer(): Boolean = true
  override def result(result: CompilerRuntime): ValType = v

  override def constantLength(runtime: CompilerRuntime): Option[Long] = Some(values.size)
}
