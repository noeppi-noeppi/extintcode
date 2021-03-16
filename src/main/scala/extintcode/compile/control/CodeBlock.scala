package extintcode.compile.control

import extintcode.asm.{AssemblyData, AssemblyText}
import extintcode.compile.{CompilerRuntime, ImportTable, LangStatement}

import scala.collection.mutable.ListBuffer

class CodeBlock(val statements: List[LangStatement]) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    runtime.pushScope()
    val text = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    for (statement <- statements) {
      val (c, d) = statement.code(imports, runtime)
      text.addAll(c)
      data.addAll(d)
    }
    runtime.popScope()
    (text.toList, data.toList)
  }
}
