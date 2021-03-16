package extintcode.compile.array

import extintcode.asm.{AssemblyData, AssemblyText, Direct, SpecialValue, StmtAdd, StmtLoad, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}
import extintcode.util.IntCodeRuntime

import scala.collection.mutable.ListBuffer

class ArrayAccess(array: LangExpression, idx: LangExpression) extends LangExpression {
  
  private var v: ValType = _
  
  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val text = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    val (c, d) = array.code(imports, runtime)
    text.addAll(c)
    data.addAll(d)
    runtime.checkType("array access", expected = true, array.pointer())
    idx.constantExpression(runtime) match {
      case Some(value) =>
        v = runtime.createExpressionResult()
        text.addOne(StmtAdd(array.value(runtime), Direct(value + 1, null), SpecialValue(IntCodeRuntime.Names.GLOBAL1)))
        text.addOne(StmtLoad(SpecialValue(IntCodeRuntime.Names.GLOBAL1), v))
      case None =>
        val (ci, di) = idx.code(imports, runtime)
        text.addAll(ci)
        data.addAll(di)
        runtime.checkType("array index", expected = false, idx.pointer())
        v = runtime.createExpressionResult()
        text.addOne(StmtAdd(array.value(runtime), idx.value(runtime), SpecialValue(IntCodeRuntime.Names.GLOBAL1)))
        text.addOne(StmtAdd(SpecialValue(IntCodeRuntime.Names.GLOBAL1), Direct(1, null), SpecialValue(IntCodeRuntime.Names.GLOBAL1)))
        text.addOne(StmtLoad(SpecialValue(IntCodeRuntime.Names.GLOBAL1), v))
    }
    (text.toList, data.toList)
  }

  override def pointer(): Boolean = false
  override protected def result(runtime: CompilerRuntime): ValType = v
}
