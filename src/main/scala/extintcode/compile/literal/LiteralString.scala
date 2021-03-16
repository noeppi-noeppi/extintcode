package extintcode.compile.literal

import extintcode.asm.{AssemblyData, AssemblyText, DataByValue, DataIntArray, DataString, DirectData, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}

import scala.collection.mutable.ListBuffer

class LiteralString(value: String) extends LangExpression {

  private var v: ValType = _

  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val name = runtime.newDataEntry("literal")
    val data = ListBuffer(
      DataByValue(name, DataString(value))
    )
    v = DirectData(name)
    (Nil, data.toList)
  }

  override def pointer(): Boolean = true
  override def result(result: CompilerRuntime): ValType = v
}
