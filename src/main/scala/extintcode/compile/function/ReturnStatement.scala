package extintcode.compile.function

import extintcode.asm.{AssemblyData, AssemblyText, SpecialValue, StmtJmp, StmtMov}
import extintcode.compile.frame.ReturnFrame
import extintcode.compile.literal.LiteralVoid
import extintcode.compile.{CodePathCheckResult, CodePathChecker, CompilerRuntime, ImportTable, LangExpression, LangStatement}
import extintcode.util.IntCodeRuntime

import scala.collection.mutable.ListBuffer

class ReturnStatement(value: LangExpression) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    if (runtime.stackSectionResultType.isEmpty) throw new IllegalStateException("Return statements can only be used inside a function.")
    val text = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    
    // Skip mov to RETURN if returning void and not returning a pointer
    val needsExpression = runtime.stackSectionResultType.get || value != LiteralVoid
    if (needsExpression) {
      text.addAll(runtime.startExpressionSection())
      val (t, d) = value.code(imports, runtime)
      text.addAll(t)
      data.addAll(d)
      runtime.checkType("value to return", runtime.stackSectionResultType.get, value.pointer())
      text.addOne(StmtMov(value.value(runtime), SpecialValue(IntCodeRuntime.Names.RETURN)))
    }
    val backjump = runtime.getVariable("~backjump")
    text.addOne(ReturnFrame(runtime.stackSectionResultType.get))
    text.addOne(StmtJmp(backjump.location))
    if (needsExpression) {
      text.addAll(runtime.endExpressionSection())
    }
    (text.toList, data.toList)
  }

  override def checkCodePath(): CodePathCheckResult = CodePathChecker.yes()
}
