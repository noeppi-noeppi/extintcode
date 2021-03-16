package extintcode.compile.operator

import extintcode.compile.{CompilerRuntime, LangExpression}

abstract class OperatorBase(protected val op1: LangExpression, protected val op2: LangExpression) extends LangExpression {
  
  protected def constantExpression(l1: Long, l2: Long): Option[Long]
  
  override final def constantExpression(runtime: CompilerRuntime): Option[Long] = op1.constantExpression(runtime)
    .flatMap(l1 => op2.constantExpression(runtime)
      .flatMap(l2 => constantExpression(l1, l2))
    )
}
