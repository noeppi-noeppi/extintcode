package extintcode.compile.function

import extintcode.asm.{AssemblyData, AssemblyText, CodeFunctionLabel, CodeLabel, Direct, IntCodeAssembler, SpecialValue, StmtJmp, StmtMov}
import extintcode.compile.{CompilerRuntime, ImportTable, LangStatement}
import extintcode.util.{FunctionEntry, IntCodeRuntime}

import scala.collection.mutable.ListBuffer

class FunctionDefinition(val name: String, val pointerReturn: Boolean, params: List[(String, Boolean)], statements: List[LangStatement]) {
  
  val label: String = "local_func_" + name + "_" + params.size + (if (pointerReturn) "p" else "d")
  val paramAmount: Int = params.size
  val entry: FunctionEntry = FunctionEntry(name, pure = false, pointerReturn, params.map(_._2))
  
  def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val text = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    runtime.specialLabel(label)
    text.addOne(CodeFunctionLabel(label, entry))
    runtime.startStackSection(pointerReturn)
    val backjump = runtime.createVariable("~backjump", pointer = false)
    text.addOne(StmtMov(SpecialValue(IntCodeRuntime.Names.BACKJUMP), backjump.location))
    for (((param, pointer), i) <- params.zipWithIndex) {
      val paramVar = runtime.createVariable(param, pointer)
      paramVar.noConst()
      text.addOne(StmtMov(SpecialValue(IntCodeAssembler.paramName0Based(i)), paramVar.location))
    }
    for (statement <- statements) {
      val (c, d) = statement.code(imports, runtime)
      text.addAll(c)
      data.addAll(d)
    }
    text.addOne(StmtMov(Direct(0, null), SpecialValue(IntCodeRuntime.Names.RETURN)))
    text.addOne(StmtJmp(backjump.location))
    runtime.endStackSection()
    (text.toList, data.toList)
  }
}
