package extintcode.compile.function

import extintcode.asm._
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
    text.addAll(runtime.startStackSection(pointerReturn))
    text.addOne(CodeFunctionLabel(label, entry))
    val (backjump, backjumpFrames) = runtime.createVariable("~backjump", pointer = false)
    text.addAll(backjumpFrames)
    text.addOne(StmtMov(SpecialValue(IntCodeRuntime.Names.BACKJUMP), backjump.location))
    for (((param, pointer), i) <- params.zipWithIndex) {
      val (paramVar, paramFrames) = runtime.createVariable(param, pointer)
      paramVar.noConst()
      text.addAll(paramFrames)
      text.addOne(StmtMov(SpecialValue(IntCodeAssembler.paramName0Based(i)), paramVar.location))
    }
    for (statement <- statements) {
      val (c, d) = statement.code(imports, runtime)
      text.addAll(c)
      data.addAll(d)
    }
    text.addOne(StmtMov(Direct(0, null), SpecialValue(IntCodeRuntime.Names.RETURN)))
    text.addOne(StmtJmp(backjump.location))
    text.addAll(runtime.endStackSection())
    (text.toList, data.toList)
  }
}
