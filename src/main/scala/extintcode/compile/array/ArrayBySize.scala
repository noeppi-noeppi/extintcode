package extintcode.compile.array

import extintcode.asm._
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}
import extintcode.util.IntCodeRuntime

import scala.collection.mutable.ListBuffer

class ArrayBySize(size: LangExpression) extends LangExpression {

  private var v: ValType = _

  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    size.constantExpression(runtime) match {
      case Some(value) if !runtime.stackSection() =>
        val name = runtime.newDataEntry("literal")
        val data = List(
          DataBySize(name, value.toInt)
        )
        v = DirectData(name)
        (Nil, data)
      case Some(value) =>
        val text = ListBuffer[AssemblyText]()
        val data = ListBuffer[AssemblyData]()
        v = runtime.createExpressionResult()
        text.addOne(StmtDyn(Direct(value + 1, null), v))
        text.addOne(StmtStore(Direct(value, null), v))
        (text.toList, data.toList)
      case None =>
        val text = ListBuffer[AssemblyText]()
        val data = ListBuffer[AssemblyData]()
        val (c, d) = size.code(imports, runtime)
        text.addAll(c)
        data.addAll(d)
        runtime.checkType("array creation", expected = false, size.pointer())
        v = runtime.createExpressionResult()
        text.addOne(StmtAdd(size.value(runtime), Direct(1, null), SpecialValue(IntCodeRuntime.Names.GLOBAL1)))
        text.addOne(StmtDyn(SpecialValue(IntCodeRuntime.Names.GLOBAL1), v))
        text.addOne(StmtStore(size.value(runtime), v))
        (text.toList, data.toList)
    }
  }

  override def pointer(): Boolean = true
  override def result(result: CompilerRuntime): ValType = v

  override def constantLength(runtime: CompilerRuntime): Option[Long] = size.constantExpression(runtime)
}
