package extintcode.compile.function

import extintcode.asm.{AssemblyData, AssemblyText, IntCodeAssembler, SpecialValue, StmtMov, ValType}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression}
import extintcode.util.IntCodeRuntime

import scala.collection.mutable.ListBuffer

class FunctionCallExpression(module: Option[String], name: String, params: List[LangExpression]) extends LangExpression {

  private var p: Boolean = false
  private var v: ValType = _
  
  override def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val func = imports.getFunc(module, name, params.size)
    val code = ListBuffer[AssemblyText]()
    val (call, data) = CompilerFunctions.call(imports, runtime, func, false, params: _*)
    code.addAll(call)
    p = func._1.pointerReturn
    // createExpressionResult is called that late as this saves us one int on the stack
    v = runtime.createExpressionResult()
    code.addOne(StmtMov(SpecialValue(IntCodeRuntime.Names.RETURN), v))
    (code.toList, data)
  }

  override def pointer(): Boolean = p
  override protected def result(result: CompilerRuntime): ValType = v
}
