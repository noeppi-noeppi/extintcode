package extintcode.compile.control

import extintcode.asm._
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression, LangStatement}

import scala.collection.mutable.ListBuffer

class ControlIf(condition: LangExpression, ifTrue: List[LangStatement], ifFalse: List[LangStatement]) extends LangStatement {

  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    if (ifTrue.isEmpty && ifFalse.isEmpty) println("Warning: pointless if condition: Both branches are empty.")
    else if (ifFalse.isEmpty) println("Warning: If condition with only else branch. You should flip the condition.")
    val text = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    runtime.startExpressionSection()
    val (cc, dc) = condition.code(imports, runtime)
    text.addAll(cc)
    data.addAll(dc)
    runtime.checkType("if condition", expected = false, condition.pointer())
    runtime.endExpressionSection()
    val endLabel = runtime.newLabel("endif")
    val elseLabel = if (ifFalse.isEmpty) endLabel else runtime.newLabel("else")
    text.addOne(StmtJz(condition.value(runtime), DirectLabel(elseLabel)))
    runtime.pushScope()
    for (statement <- ifTrue) {
      val (c, d) = statement.code(imports, runtime)
      text.addAll(c)
      data.addAll(d)
    }
    runtime.popScope()
    if (ifFalse.nonEmpty) {
      text.addOne(StmtJmp(DirectLabel(endLabel)))
      text.addOne(CodeLabel(elseLabel))
      runtime.pushScope()
      for (statement <- ifFalse) {
        val (c, d) = statement.code(imports, runtime)
        text.addAll(c)
        data.addAll(d)
      }
      runtime.popScope()
    }
    text.addOne(CodeLabel(endLabel))
    (text.toList, data.toList)
  }
}
