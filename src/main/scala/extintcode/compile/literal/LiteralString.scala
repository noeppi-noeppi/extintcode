package extintcode.compile.literal

import extintcode.asm.{AssemblyData, AssemblyText, DataByValue, DataIntArray, DataString, DirectData, IntCodeAssembler, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}

import scala.collection.mutable.ListBuffer

class LiteralString(value: String) extends LangExpression {

  private var v: ValType = _

  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    v = DirectData(runtime.createStringLiteral(value))
    (Nil, Nil)
  }

  override def pointer(): Boolean = true
  override def result(result: CompilerRuntime): ValType = v

  override def constantLength(runtime: CompilerRuntime): Option[Long] = Some(IntCodeAssembler.stringToCodePoints(value).size)
}
