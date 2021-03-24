package extintcode.compile.operator2

import extintcode.asm.{AssemblyData, AssemblyText, Direct, StmtMul, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}

import scala.collection.mutable.ListBuffer

class Negation(value: LangExpression) extends LangExpression {
  
  private var v: ValType = _
  
  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val text = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    val (c, d) = value.code(imports, runtime)
    text.addAll(c)
    data.addAll(d)
    runtime.checkType("negation", expected = false, value.pointer())
    v = runtime.createExpressionResult()
    text.addOne(StmtMul(value.value(runtime), Direct(-1, null), v))
    (text.toList, data.toList)
  }

  override def pointer(): Boolean = false
  override protected def result(runtime: CompilerRuntime): ValType = v

  override def constantExpression(runtime: CompilerRuntime): Option[Long] = value.constantExpression(runtime).map(x => -x)
}
