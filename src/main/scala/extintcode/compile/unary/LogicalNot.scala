package extintcode.compile.unary

import extintcode.asm.{AssemblyData, AssemblyText, Direct, StmtEq, StmtMul, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}

import scala.collection.mutable.ListBuffer

class LogicalNot(value: LangExpression) extends LangExpression {

  private var v: ValType = _

  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val text = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    val (c, d) = value.code(imports, runtime)
    text.addAll(c)
    data.addAll(d)
    runtime.checkType("logical not", expected = false, value.pointer())
    v = runtime.createExpressionResult()
    text.addOne(StmtEq(value.value(runtime), Direct(0, null), v))
    (text.toList, data.toList)
  }

  override def pointer(): Boolean = false
  override protected def result(runtime: CompilerRuntime): ValType = v

  override def constantExpression(runtime: CompilerRuntime): Option[Long] = value.constantExpression(runtime).map(x => if (x == 0) 1 else 0)
}
