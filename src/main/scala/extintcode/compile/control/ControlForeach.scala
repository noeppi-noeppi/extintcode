package extintcode.compile.control

import extintcode.asm.{AssemblyData, AssemblyText, CodeLabel, Direct, DirectLabel, SpecialValue, StmtAdd, StmtJmp, StmtJz, StmtLoad, StmtLt, StmtMov}
import extintcode.compile.{CodePathCheckResult, CodePathChecker, CompilerRuntime, ControlJumps, ImportTable, LangExpression, LangStatement}
import extintcode.util.IntCodeRuntime

import scala.collection.mutable.ListBuffer

class ControlForeach(varName: String, varType: Boolean, indexVarName: Option[String], array: LangExpression, statements: List[LangStatement]) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    if (statements.isEmpty) println("Warning: Empty foreach loop.")
    
    // We need a stack section for internal variables
    val startStack = !runtime.stackSection()
    if (startStack) runtime.anonStackSection()
    
    val text = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    val endLabel = runtime.newLabel("endforeach")
    val loopLabel = runtime.newLabel("loop")
    
    text.addAll(runtime.pushScope())
    text.addAll(runtime.pushScope())
    runtime.startExpressionSection()
    val (ta, da) = array.code(imports, runtime)
    text.addAll(ta)
    data.addAll(da)
    runtime.endExpressionSection()
    runtime.checkType("foreach array", expected = true, actual = array.pointer())
    text.addAll(runtime.popScope())

    val (arrayVar, varFrames1) = runtime.createInternalVariable("foreach_array", pointer = true)
    val (idxVar, varFrames2) = indexVarName match {
      case Some(name) => runtime.createVariable(name, pointer = false)
      case None => runtime.createInternalVariable("foreach_index", pointer = false)
    }
    val (elemVar, varFrames3) = runtime.createVariable(varName, varType)
    // Cache the size
    val (sizeVar, varFrames4) = runtime.createInternalVariable("foreach_size", pointer = false)
    text.addAll(varFrames1)
    text.addAll(varFrames2)
    text.addAll(varFrames3)
    text.addAll(varFrames4)

    arrayVar.noConst()
    idxVar.noConst()
    elemVar.noConst()

    text.addOne(StmtMov(array.value(runtime), arrayVar.location))

    text.addAll(runtime.pushScope())
    text.addOne(StmtMov(Direct(0, null), idxVar.location))
    text.addOne(StmtLoad(arrayVar.location, sizeVar.location))
    text.addOne(CodeLabel(loopLabel))
    text.addOne(StmtLt(idxVar.location, sizeVar.location, SpecialValue(IntCodeRuntime.Names.GLOBAL1)))
    text.addOne(StmtJz(SpecialValue(IntCodeRuntime.Names.GLOBAL1), DirectLabel(endLabel)))
    text.addOne(StmtAdd(idxVar.location, Direct(1, null), SpecialValue(IntCodeRuntime.Names.GLOBAL1)))
    text.addOne(StmtAdd(arrayVar.location, SpecialValue(IntCodeRuntime.Names.GLOBAL1), SpecialValue(IntCodeRuntime.Names.GLOBAL1)))
    text.addOne(StmtLoad(SpecialValue(IntCodeRuntime.Names.GLOBAL1), elemVar.location))
    runtime.pushControl(ControlJumps(Left(DirectLabel(endLabel)), Left(DirectLabel(loopLabel)), Right("next is not available in foreach loop. Use a while loop instead.")))
    for (statement <- statements) {
      val (ta, da) = statement.code(imports, runtime)
      text.addAll(ta)
      data.addAll(da)
    }
    runtime.popControl()
    text.addOne(StmtAdd(idxVar.location, Direct(1, null), idxVar.location))
    text.addAll(runtime.popScope())
    text.addAll(runtime.popScope())
    text.addOne(StmtJmp(DirectLabel(loopLabel)))
    text.addOne(CodeLabel(endLabel))

    if (startStack) runtime.endStackSection()
    
    (text.toList, data.toList)
  }

  override def checkCodePath(): CodePathCheckResult = CodePathChecker.children(this, this.statements)
}
