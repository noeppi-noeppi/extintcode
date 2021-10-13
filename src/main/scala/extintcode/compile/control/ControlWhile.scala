package extintcode.compile.control

import extintcode.asm.{AssemblyData, AssemblyText, CodeLabel, DirectLabel, StmtJmp, StmtJz}
import extintcode.compile.{CodePathCheckResult, CodePathChecker, CompilerRuntime, ControlJumps, ImportTable, LangExpression, LangStatement}

import scala.collection.mutable.ListBuffer

class ControlWhile(condition: LangExpression, statements: List[LangStatement]) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    if (statements.isEmpty) println("Warning: Empty while loop.")
    val text = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    val endLabel = runtime.newLabel("endwhile")
    val loopLabel = runtime.newLabel("loop")
    val nextLabel = runtime.newLabel("next")
    text.addOne(CodeLabel(loopLabel))
    text.addAll(runtime.startExpressionSection())
    val (cc, dc) = condition.code(imports, runtime)
    text.addAll(cc)
    data.addAll(dc)
    runtime.checkType("while condition", expected = false, condition.pointer())
    text.addAll(runtime.endExpressionSection())
    text.addOne(StmtJz(condition.value(runtime), DirectLabel(endLabel)))
    text.addOne(CodeLabel(nextLabel))
    text.addAll(runtime.pushScope())
    runtime.pushControl(ControlJumps(Left(DirectLabel(endLabel)), Left(DirectLabel(loopLabel)), Left(DirectLabel(nextLabel))))
    for (statement <- statements) {
      val (c, d) = statement.code(imports, runtime)
      text.addAll(c)
      data.addAll(d)
    }
    runtime.popControl()
    text.addAll(runtime.popScope())
    text.addOne(StmtJmp(DirectLabel(loopLabel)))
    text.addOne(CodeLabel(endLabel))
    (text.toList, data.toList)
  }

  override def checkCodePath(): CodePathCheckResult = CodePathChecker.children(this, statements)
}
