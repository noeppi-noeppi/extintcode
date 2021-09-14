package extintcode.compile.function

import extintcode.asm.{AssemblyData, AssemblyText, Direct, IntCodeAssembler, SpecialValue, StmtCall, StmtMov, StmtPop, StmtPush, ValType}
import extintcode.compile.frame.CallFrame
import extintcode.compile.{BuiltinHeader, CompilerRuntime, ImportTable, LangExpression}
import extintcode.util.FunctionEntry

import scala.collection.mutable.ListBuffer

object CompilerFunctions {
  
  def call(imports: ImportTable, runtime: CompilerRuntime, func: (String, FunctionEntry, ValType), statement: Boolean, params: LangExpression*): (List[AssemblyText], List[AssemblyData]) = {
    val (module, function, memory) = func
    if (function.args != params.size) {
      throw new IllegalArgumentException("Internal Compiler Error: Calling function with incorrect amount of arguments.")
    }
    val text = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    if (statement) text.addAll(runtime.startExpressionSection())
    for (i <- function.signature.indices) {
      val (t, d) = params(i).code(imports, runtime)
      text.addAll(t)
      data.addAll(d)
    }
    BuiltinHeader.specialFunctionCode(func, function.signature.indices.map(params(_).value(runtime)): _*) match {
      case Some(x) =>
        text.addAll(x)
        if (statement) text.addAll(runtime.endExpressionSection())
      case None =>
        for (i <- function.signature.indices) {
          runtime.checkType(if (statement) "function statement" else "function expression", function.signature(i), params(i).pointer())
          text.addOne(StmtMov(params(i).value(runtime), SpecialValue(IntCodeAssembler.paramName0Based(i))))
        }
        if (statement) text.addAll(runtime.endExpressionSection())
        val stack = runtime.stackSizeForPush(function.pure, statement)
        text.addAll(stack.map(x => StmtPush(Direct(x, null))))
        text.addOne(CallFrame(module, function))
        text.addOne(StmtCall(memory))
        text.addAll(stack.map(x => StmtPop(Direct(x, null))))
    }
    (text.toList, data.toList)
  }
}
