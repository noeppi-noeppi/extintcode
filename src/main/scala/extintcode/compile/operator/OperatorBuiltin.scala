package extintcode.compile.operator

import extintcode.asm.{AssemblyData, AssemblyText, Direct, SpecialValue, StmtAdd, StmtEq, StmtLt, StmtMul, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}
import extintcode.util.IntCodeRuntime

import scala.collection.mutable.ListBuffer

abstract class OperatorBuiltin(
                       name: String,
                       pointersAllowed: Boolean,
                       factory: (CompilerRuntime, OpValues) => List[AssemblyText],
                       op1: LangExpression,
                       op2: LangExpression
                     ) extends OperatorBase(op1, op2) {
  
  private var v: ValType = _
  
  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    v = runtime.createExpressionResult()
    val code = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    val (c1, d1) = op1.code(imports, runtime)
    val (c2, d2) = op2.code(imports, runtime)
    code.addAll(c1)
    data.addAll(d1)
    code.addAll(c2)
    data.addAll(d2)
    if (pointersAllowed) {
      runtime.checkType(name + " operator (types must be equal)", expected = op1.pointer(), actual = op2.pointer())
    } else {
      runtime.checkType(name + " operator (1)", expected = false, actual = op1.pointer())
      runtime.checkType(name + " operator (2)", expected = false, actual = op2.pointer())
    }
    code.addAll(factory(runtime, OpValues(op1.value(runtime), op2.value(runtime), v)))
    (code.toList, data.toList)
  }

  override def pointer(): Boolean = false
  override protected def result(result: CompilerRuntime): ValType = v
}

class OperatorAdd(op1: LangExpression, op2: LangExpression) extends OperatorBuiltin(
  "Addition", false, (_, v) => List(StmtAdd(v.op1, v.op2, v.result)),
  op1, op2
) {
  override protected def constantExpression(l1: Long, l2: Long): Option[Long] = Some(l1 + l2)
}

class OperatorMul(op1: LangExpression, op2: LangExpression) extends OperatorBuiltin(
  "Multiplication", false, (_, v) => List(StmtMul(v.op1, v.op2, v.result)),
  op1, op2
) {
  override protected def constantExpression(l1: Long, l2: Long): Option[Long] = Some(l1 * l2)
}

class OperatorSub(op1: LangExpression, op2: LangExpression) extends OperatorBuiltin(
  "Subtraction", false, (runtime, v) => {
    val neg = runtime.negateCompileTime(v.op2)
    if (neg.isDefined) {
      List(StmtAdd(v.op1, neg.get, v.result))
    } else {
      List(
        StmtMul(v.op2, Direct(-1, null), SpecialValue(IntCodeRuntime.Names.GLOBAL1)),
        StmtAdd(v.op1, SpecialValue(IntCodeRuntime.Names.GLOBAL1), v.result)
      )
    }
  },
  op1, op2
) {
  override protected def constantExpression(l1: Long, l2: Long): Option[Long] = Some(l1 - l2)
}

class OperatorEq(op1: LangExpression, op2: LangExpression) extends OperatorBuiltin(
  "Equality", true, (_, v) => List(StmtEq(v.op1, v.op2, v.result)),
  op1, op2
) {
  override protected def constantExpression(l1: Long, l2: Long): Option[Long] = Some(l1 == l2).map(if (_) 1 else 0)
}

class OperatorNeq(op1: LangExpression, op2: LangExpression) extends OperatorBuiltin(
  "Inverted Equality", true, (_, v) => List(
    StmtEq(v.op1, v.op2, SpecialValue(IntCodeRuntime.Names.GLOBAL1)),
    StmtEq(SpecialValue(IntCodeRuntime.Names.GLOBAL1), Direct(0, null), v.result)
  ),
  op1, op2
) {
  override protected def constantExpression(l1: Long, l2: Long): Option[Long] = Some(l1 != l2).map(if (_) 1 else 0)
}

class OperatorLt(op1: LangExpression, op2: LangExpression) extends OperatorBuiltin(
  "Lower Than", false, (_, v) => List(StmtLt(v.op1, v.op2, v.result)),
  op1, op2
) {
  override protected def constantExpression(l1: Long, l2: Long): Option[Long] = Some(l1 < l2).map(if (_) 1 else 0)
}

class OperatorGt(op1: LangExpression, op2: LangExpression) extends OperatorBuiltin(
  "Greater Than", false, (_, v) => List(StmtLt(v.op2, v.op1, v.result)),
  op1, op2
) {
  override protected def constantExpression(l1: Long, l2: Long): Option[Long] = Some(l1 > l2).map(if (_) 1 else 0)
}

class OperatorLe(op1: LangExpression, op2: LangExpression) extends OperatorBuiltin(
  "Lower Equal", false, (_, v) => List(
    StmtLt(v.op2, v.op1, SpecialValue(IntCodeRuntime.Names.GLOBAL1)),
    StmtEq(SpecialValue(IntCodeRuntime.Names.GLOBAL1), Direct(0, null), v.result)
  ),
  op1, op2
) {
  override protected def constantExpression(l1: Long, l2: Long): Option[Long] = Some(l1 <= l2).map(if (_) 1 else 0)
}

class OperatorGe(op1: LangExpression, op2: LangExpression) extends OperatorBuiltin(
  "Greater Equal", false, (_, v) => List(
    StmtLt(v.op1, v.op2, SpecialValue(IntCodeRuntime.Names.GLOBAL1)),
    StmtEq(SpecialValue(IntCodeRuntime.Names.GLOBAL1), Direct(0, null), v.result)
  ),
  op1, op2
) {
  override protected def constantExpression(l1: Long, l2: Long): Option[Long] = Some(l1 >= l2).map(if (_) 1 else 0)
}