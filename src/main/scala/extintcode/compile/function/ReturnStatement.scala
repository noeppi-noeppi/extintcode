package extintcode.compile.function

import extintcode.asm.{AssemblyData, AssemblyText, SpecialValue, StmtJmp, StmtMov}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression, LangStatement}
import extintcode.util.IntCodeRuntime

import scala.collection.mutable.ListBuffer

class ReturnStatement(value: LangExpression) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    if (runtime.stackSectionResultType.isEmpty) throw new IllegalStateException("Return statements can only be used inside a function.")
    val text = ListBuffer[AssemblyText]()
    text.addAll(runtime.startExpressionSection())
    val (t, data) = value.code(imports, runtime)
    text.addAll(t)
    text.addAll(runtime.endExpressionSection())
    runtime.checkType("value to return", runtime.stackSectionResultType.get, value.pointer())
    val backjump = runtime.getVariable("~backjump")
    text.addOne(StmtMov(value.value(runtime), SpecialValue(IntCodeRuntime.Names.RETURN)))
    text.addOne(StmtJmp(backjump.location))
    (text.toList, data)
  }
}
