package extintcode.compile.operator

import extintcode.asm.{AssemblyData, AssemblyText, CodeLabel, Direct, DirectLabel, SpecialValue, StmtAdd, StmtEq, StmtJnz, StmtJz, StmtMov, StmtMul, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}
import extintcode.util.IntCodeRuntime

import scala.collection.mutable.ListBuffer

abstract class OperatorLazyEval(
                        name: String,
                        lazyCheck: (CompilerRuntime, ValType, ValType, String) => List[AssemblyText],
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
    code.addAll(c1)
    data.addAll(d1)
    val label = runtime.newLabel("lazy")
    code.addAll(lazyCheck(runtime, op1.value(runtime), v, label))
    val (c2, d2) = op2.code(imports, runtime)
    code.addAll(c2)
    data.addAll(d2)
    runtime.checkType(name + " operator (1)", expected = false, actual = op1.pointer())
    runtime.checkType(name + " operator (2)", expected = false, actual = op2.pointer())
    code.addAll(factory(runtime, OpValues(op1.value(runtime), op2.value(runtime), v)))
    code.addOne(CodeLabel(label))
    (code.toList, data.toList)
  }

  override def pointer(): Boolean = false
  override protected def result(result: CompilerRuntime): ValType = v
}

class OperatorAnd(op1: LangExpression, op2: LangExpression) extends OperatorLazyEval(
  "Logical And",
  (_, in1, v, label) => List(StmtMov(Direct(0, null), v), StmtJz(in1, DirectLabel(label))),
  (_, v) => List(StmtMul(v.op1, v.op2, v.result)),
  op1, op2
) {
  override protected def constantExpression(l1: Long, l2: Long): Option[Long] = Some((l1 != 0) && (l2 != 0)).map(if (_) 1 else 0)
}

class OperatorOr(op1: LangExpression, op2: LangExpression) extends OperatorLazyEval(
  "Logical Or",
  (_, in1, v, label) => List(StmtMov(Direct(1, null), v), StmtJnz(in1, DirectLabel(label))),
  (_, v) => List(
    StmtEq(v.op1, Direct(0, null), SpecialValue(IntCodeRuntime.Names.GLOBAL1)),
    StmtEq(v.op2, Direct(0, null), SpecialValue(IntCodeRuntime.Names.GLOBAL2)),
    StmtMul(SpecialValue(IntCodeRuntime.Names.GLOBAL1), SpecialValue(IntCodeRuntime.Names.GLOBAL2), SpecialValue(IntCodeRuntime.Names.GLOBAL1)),
    StmtEq(SpecialValue(IntCodeRuntime.Names.GLOBAL1), Direct(0, null), v.result)
  ),
  op1, op2
) {
  override protected def constantExpression(l1: Long, l2: Long): Option[Long] = Some((l1 != 0) || (l2 != 0)).map(if (_) 1 else 0)
}