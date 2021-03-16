package extintcode.compile

import extintcode.asm.{AssemblyData, AssemblyText, ValType}

trait LangStatement {
  
  def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData])
}