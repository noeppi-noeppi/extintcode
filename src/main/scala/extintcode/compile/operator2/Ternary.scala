package extintcode.compile.operator2

import extintcode.asm._
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}

import scala.collection.mutable.ListBuffer

class Ternary(condition: LangExpression, ifTrue: LangExpression, ifFalse: LangExpression) extends LangExpression {

  private var v: ValType = _
  private var p: Boolean = false

  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    condition.constantExpression(runtime) match {
      case Some(x) =>
        val expr = if (x != 0) ifTrue else ifFalse
        val code = expr.code(imports, runtime)
        v = expr.value(runtime)
        p = expr.pointer()
        code
      case None =>
        val labelFalse = runtime.newLabel("ternary_false")
        val labelEnd = runtime.newLabel("ternary_end")
        val text = ListBuffer[AssemblyText]()
        val data = ListBuffer[AssemblyData]()
        val (cc, dc) = condition.code(imports, runtime)
        text.addAll(cc)
        data.addAll(dc)
        v = runtime.createExpressionResult()
        text.addOne(StmtJz(condition.value(runtime), DirectLabel(labelFalse)))
        val (ct, dt) = ifTrue.code(imports, runtime)
        text.addAll(ct)
        data.addAll(dt)
        text.addOne(StmtMov(ifTrue.value(runtime), v))
        text.addOne(StmtJmp(DirectLabel(labelEnd)))
        text.addOne(CodeLabel(labelFalse))
        val (cf, df) = ifFalse.code(imports, runtime)
        text.addAll(cf)
        data.addAll(df)
        text.addOne(StmtMov(ifFalse.value(runtime), v))
        text.addOne(CodeLabel(labelEnd))
        runtime.checkType("ternary condition", expected = false, condition.pointer())
        runtime.checkType("ternary operator (result expressions must have the same type)", ifTrue.pointer(), ifTrue.pointer())
        p = ifTrue.pointer()
        (text.toList, data.toList)
    }
  }

  override def pointer(): Boolean = p

  override protected def result(runtime: CompilerRuntime): ValType = v

  override def constantExpression(runtime: CompilerRuntime): Option[Long] = {
    condition.constantExpression(runtime).flatMap(x =>
      if (x != 0) ifTrue.constantExpression(runtime) else ifFalse.constantExpression(runtime)
    )
  }
}
