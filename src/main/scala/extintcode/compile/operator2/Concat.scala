package extintcode.compile.operator2

import extintcode.asm.{AssemblyData, AssemblyText, CodeLabel, Direct, DirectLabel, SpecialValue, StmtAdd, StmtDyn, StmtJmp, StmtJz, StmtLoad, StmtMov, StmtStore, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}
import extintcode.util.IntCodeRuntime

import scala.collection.mutable.ListBuffer

class Concat(op1: LangExpression, op2: LangExpression) extends LangExpression {
  
  // Resolve children of type Concat to only concatenate once
  private lazy val expressionList: List[LangExpression] = List(op1, op2).flatMap {
    case child: Concat => child.expressionList
    case child => Some(child)
  }
  
  private var v: ValType = _
  
  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val text = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    for (expr <- expressionList) {
      val (t, d) = expr.code(imports, runtime)
      text.addAll(t)
      data.addAll(d)
    }
    expressionList.foreach(expr => runtime.checkType("Array concatenation", expected = true, actual = expr.pointer()))
    val lengths = expressionList.map(_.constantLength(runtime))
    val results = expressionList.map(_.value(runtime))
    val constantPart = lengths.flatten.sum
    
    v = runtime.createExpressionResult()
    if (lengths.exists(_.isEmpty)) {
      text.addOne(StmtMov(Direct(constantPart, null), SpecialValue(IntCodeRuntime.Names.GLOBAL1)))
      for (idx <- lengths.zipWithIndex.filter(_._1.isEmpty).map(_._2)) {
        text.addOne(StmtLoad(results(idx), SpecialValue(IntCodeRuntime.Names.GLOBAL2)))
        text.addOne(StmtAdd(SpecialValue(IntCodeRuntime.Names.GLOBAL1), SpecialValue(IntCodeRuntime.Names.GLOBAL2), SpecialValue(IntCodeRuntime.Names.GLOBAL1)))
      }
      text.addOne(StmtAdd(SpecialValue(IntCodeRuntime.Names.GLOBAL1), Direct(1, null), SpecialValue(IntCodeRuntime.Names.GLOBAL2)))
      text.addOne(StmtDyn(SpecialValue(IntCodeRuntime.Names.GLOBAL2), v))
      // Write size
      text.addOne(StmtStore(SpecialValue(IntCodeRuntime.Names.GLOBAL1), v))
    } else {
      text.addOne(StmtDyn(Direct(constantPart + 1, null), v))
      // Write size
      text.addOne(StmtStore(Direct(constantPart, null), v))
    }
    
    // We start writing at address 1 after the dyn
    text.addOne(StmtAdd(v, Direct(1, null), SpecialValue(IntCodeRuntime.Names.GLOBAL4))) // GLOBAL4 is the current index to write
    for (idx <- expressionList.indices) {
      text.addOne(StmtLoad(results(idx), SpecialValue(IntCodeRuntime.Names.GLOBAL5))) // GLOBAL5 stores iterations left
      text.addOne(StmtMov(SpecialValue(IntCodeRuntime.Names.GLOBAL5), SpecialValue(IntCodeRuntime.Names.GLOBAL6))) // GLOBAL6 stores size of current element
      val back = runtime.newLabel("concat_loop")
      val end = runtime.newLabel("concat_end")
      text.addOne(CodeLabel(back))
      text.addOne(StmtJz(SpecialValue(IntCodeRuntime.Names.GLOBAL5), DirectLabel(end)))
      // We copy each element from back to front
      // GLOBAL 5 runs from size to 1 which are the exact offsets we need.
      // Store source address in GLOBAL7
      text.addOne(StmtAdd(results(idx), SpecialValue(IntCodeRuntime.Names.GLOBAL5), SpecialValue(IntCodeRuntime.Names.GLOBAL7)))
      // Store value in GLOBAL8
      text.addOne(StmtLoad(SpecialValue(IntCodeRuntime.Names.GLOBAL7), SpecialValue(IntCodeRuntime.Names.GLOBAL8)))
      // Decrement GLOBAL5 now as we need it to be 0 based instead of 1 based to calculate target address
      text.addOne(StmtAdd(SpecialValue(IntCodeRuntime.Names.GLOBAL5), Direct(-1, null), SpecialValue(IntCodeRuntime.Names.GLOBAL5)))
      // Store target address in GLOBAL7
      text.addOne(StmtAdd(SpecialValue(IntCodeRuntime.Names.GLOBAL4), SpecialValue(IntCodeRuntime.Names.GLOBAL5), SpecialValue(IntCodeRuntime.Names.GLOBAL7)))
      // Store the value
      text.addOne(StmtStore(SpecialValue(IntCodeRuntime.Names.GLOBAL8), SpecialValue(IntCodeRuntime.Names.GLOBAL7)))
      text.addOne(StmtJmp(DirectLabel(back)))
      text.addOne(CodeLabel(end))
      // Increase base address by copied bits
      text.addOne(StmtAdd(SpecialValue(IntCodeRuntime.Names.GLOBAL4), SpecialValue(IntCodeRuntime.Names.GLOBAL6), SpecialValue(IntCodeRuntime.Names.GLOBAL4)))
    }
    (text.toList, data.toList)
  }

  override def pointer(): Boolean = true
  override protected def result(runtime: CompilerRuntime): ValType = v

  override def constantLength(runtime: CompilerRuntime): Option[Long] = {
    val lengths = expressionList.map(_.constantLength(runtime))
    if (lengths.forall(_.isDefined)) {
      Some(lengths.map(_.get).sum)
    } else {
      None
    }
  }
}
