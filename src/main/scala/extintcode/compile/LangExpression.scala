package extintcode.compile

import extintcode.asm.{AssemblyData, AssemblyText, Direct, ValType}

trait LangExpression {
  
  protected def generate(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData])
  final def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val const = constantExpression(runtime)
    const.map(_ => (Nil, Nil)).getOrElse(generate(imports, runtime))
  }
  
  def pointer(): Boolean
  protected def result(runtime: CompilerRuntime): ValType
  
  final def value(runtime: CompilerRuntime): ValType = {
    val const = constantExpression(runtime)
    if (const.isDefined && pointer()) throw new IllegalStateException("Internal Compiler Error: Constant expression on pointer.")
    const.map(Direct(_, null)).getOrElse(result(runtime))
  }
  
  def constantExpression(runtime: CompilerRuntime): Option[Long] = None
  def constantLength(runtime: CompilerRuntime): Option[Long] = None
}
