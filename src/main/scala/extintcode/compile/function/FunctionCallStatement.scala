package extintcode.compile.function

import extintcode.asm.{AssemblyData, AssemblyText, IntCodeAssembler, SpecialValue}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression, LangStatement}

import scala.collection.mutable.ListBuffer

class FunctionCallStatement(module: Option[String], name: String, params: List[LangExpression]) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val func = imports.getFunc(module, name, params.size)
    CompilerFunctions.call(imports, runtime, func, true, params: _*)
  }
}
