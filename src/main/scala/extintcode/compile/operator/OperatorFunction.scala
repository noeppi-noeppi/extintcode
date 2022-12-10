package extintcode.compile.operator

import extintcode.asm._
import extintcode.compile.function.CompilerFunctions
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}
import extintcode.util.{FunctionEntry, IntCodeRuntime}

import scala.collection.mutable.ListBuffer

abstract class OperatorFunction(
                                 factory: ImportTable => (String, FunctionEntry, ValType),
                                 op1: LangExpression, op2: LangExpression
                               ) extends OperatorBase(op1, op2) {
  
  private var v: ValType = _
  
  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val func = factory(imports)
    v = runtime.createExpressionResult()
    val code = ListBuffer[AssemblyText]()
    val (call, data) = CompilerFunctions.call(imports, runtime, func, false, op1, op2)
    code.addAll(call)
    code.addOne(StmtMov(SpecialValue(IntCodeRuntime.Names.RETURN), v))
    (code.toList, data)
  }

  override def pointer(): Boolean = false
  override def result(result: CompilerRuntime): ValType = v
}

class OperatorDiv(op1: LangExpression, op2: LangExpression) extends OperatorFunction(
  imports => imports.getImplicit("stdlib", "div", 2, "Division Operator"),
  op1, op2) {
  override protected def constantExpression(l1: Long, l2: Long): Option[Long] = Some(l1 / l2)
}

class OperatorMod(op1: LangExpression, op2: LangExpression) extends OperatorFunction(
  imports => imports.getImplicit("stdlib", "mod", 2, "Modulus Operator"),
  op1, op2) {
  override protected def constantExpression(l1: Long, l2: Long): Option[Long] = Some(l1 % l2)
}

class OperatorPow(op1: LangExpression, op2: LangExpression) extends OperatorFunction(
  imports => imports.getImplicit("stdlib", "pow", 2, "Power Operator"),
  op1, op2) {
  override protected def constantExpression(l1: Long, l2: Long): Option[Long] = Some(Math.pow(l1.toDouble, l2.toDouble).toLong)
}
