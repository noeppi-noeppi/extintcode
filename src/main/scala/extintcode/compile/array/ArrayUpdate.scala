package extintcode.compile.array

import extintcode.asm._
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression, LangStatement}
import extintcode.util.IntCodeRuntime

import scala.collection.mutable.ListBuffer

class ArrayUpdate(array: LangExpression, idx: LangExpression, value: LangExpression) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val cc = idx.constantExpression(runtime)
    val text = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    text.addAll(runtime.startExpressionSection())
    val (c, d) = array.code(imports, runtime)
    text.addAll(c)
    data.addAll(d)
    runtime.checkType("array update", expected = true, array.pointer())
    if (cc.isEmpty) {
      val (ci, di) = idx.code(imports, runtime)
      text.addAll(ci)
      data.addAll(di)
      runtime.checkType("array index", expected = false, idx.pointer())
    }
    val (cv, dv) = value.code(imports, runtime)
    text.addAll(cv)
    data.addAll(dv)
    runtime.checkType("array element", expected = false, value.pointer())
    text.addAll(runtime.endExpressionSection())
    val address = cc match {
      case Some(idxValue) =>
        text.addOne(StmtAdd(array.value(runtime), Direct(idxValue + 1, null), SpecialValue(IntCodeRuntime.Names.GLOBAL1)))
        SpecialValue(IntCodeRuntime.Names.GLOBAL1)
      case None =>
        text.addOne(StmtAdd(array.value(runtime), idx.value(runtime), SpecialValue(IntCodeRuntime.Names.GLOBAL1)))
        text.addOne(StmtAdd(SpecialValue(IntCodeRuntime.Names.GLOBAL1), Direct(1, null), SpecialValue(IntCodeRuntime.Names.GLOBAL1)))
        SpecialValue(IntCodeRuntime.Names.GLOBAL1)
    }
    text.addOne(StmtStore(value.value(runtime), address))
    (text.toList, data.toList)
  }
}
