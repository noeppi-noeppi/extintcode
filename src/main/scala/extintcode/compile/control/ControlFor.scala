package extintcode.compile.control

import extintcode.asm.{AssemblyData, AssemblyText, CodeLabel, DirectLabel, StmtJmp, StmtJz}
import extintcode.compile.{CodePathCheckResult, CodePathChecker, CompilerRuntime, ControlJumps, ImportTable, LangExpression, LangStatement}

import scala.collection.mutable.ListBuffer

class ControlFor(init: Option[LangStatement], condition: LangExpression, last: Option[LangStatement], statements: List[LangStatement]) extends LangStatement {

  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    if (statements.isEmpty) println("Warning: Empty for loop.")
    val text = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    val endLabel = runtime.newLabel("endfor")
    val loopLabel = runtime.newLabel("loop")
    text.addAll(runtime.pushScope())
    if (init.isDefined) {
      val (ci, di) = init.get.code(imports, runtime)
      text.addAll(ci)
      data.addAll(di)
    }
    text.addOne(CodeLabel(loopLabel))
    text.addAll(runtime.startExpressionSection())
    val (cc, dc) = condition.code(imports, runtime)
    text.addAll(cc)
    data.addAll(dc)
    runtime.checkType("for condition", expected = false, condition.pointer())
    text.addAll(runtime.endExpressionSection())
    text.addOne(StmtJz(condition.value(runtime), DirectLabel(endLabel)))
    text.addAll(runtime.pushScope())
    runtime.pushControl(ControlJumps(Left(DirectLabel(endLabel)), Left(DirectLabel(loopLabel)), Right("next is not available in for loop. Use a while loop instead.")))
    for (statement <- statements) {
      val (c, d) = statement.code(imports, runtime)
      text.addAll(c)
      data.addAll(d)
    }
    runtime.popControl()
    text.addAll(runtime.popScope())
    if (last.isDefined) {
      val (cl, dl) = last.get.code(imports, runtime)
      text.addAll(cl)
      data.addAll(dl)
    }
    text.addAll(runtime.popScope())
    text.addOne(StmtJmp(DirectLabel(loopLabel)))
    text.addOne(CodeLabel(endLabel))
    (text.toList, data.toList)
  }

  override def checkCodePath(): CodePathCheckResult = CodePathChecker.children(this, statements)
}
