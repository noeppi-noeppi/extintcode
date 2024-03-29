package extintcode.compile.control

import extintcode.asm.{AssemblyData, AssemblyText}
import extintcode.compile.{CodePathCheckResult, CodePathChecker, CompilerRuntime, ImportTable, LangStatement}

import scala.collection.mutable.ListBuffer

class CodeBlock(val statements: List[LangStatement]) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val text = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    text.addAll(runtime.pushScope())
    for (statement <- statements) {
      val (c, d) = statement.code(imports, runtime)
      text.addAll(c)
      data.addAll(d)
    }
    text.addAll(runtime.popScope())
    (text.toList, data.toList)
  }

  override def checkCodePath(): CodePathCheckResult = CodePathChecker.children(this, statements)
}
