package extintcode.compile.control

import extintcode.asm.{AssemblyData, AssemblyText, CodeLabel, DirectLabel, StmtJmp, StmtJz}
import extintcode.compile.{CompilerRuntime, ControlJumps, ImportTable, LangExpression, LangStatement}

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
    runtime.startExpressionSection()
    val (cc, dc) = condition.code(imports, runtime)
    text.addAll(cc)
    data.addAll(dc)
    runtime.checkType("while condition", expected = false, condition.pointer())
    runtime.endExpressionSection()
    text.addOne(StmtJz(condition.value(runtime), DirectLabel(endLabel)))
    text.addOne(CodeLabel(nextLabel))
    runtime.pushScope()
    runtime.pushControl(ControlJumps(DirectLabel(endLabel), DirectLabel(loopLabel), DirectLabel(nextLabel)))
    for (statement <- statements) {
      val (c, d) = statement.code(imports, runtime)
      text.addAll(c)
      data.addAll(d)
    }
    runtime.popControl()
    runtime.popScope()
    text.addOne(StmtJmp(DirectLabel(loopLabel)))
    text.addOne(CodeLabel(endLabel))
    (text.toList, data.toList)
  }
}
