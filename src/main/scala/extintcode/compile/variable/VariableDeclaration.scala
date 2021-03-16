package extintcode.compile.variable

import extintcode.asm.{AssemblyData, AssemblyText, DataBySizeValue, DataInts, Direct, FieldLabel, MemoryData, StmtMov}
import extintcode.compile.literal.{LiteralArray, LiteralNull}
import extintcode.compile.{CompilerRuntime, ImportTable, LangExpression, LangStatement, Variable}
import extintcode.util.FieldEntry

class VariableDeclaration(name: String, pointer: Boolean, const: Boolean, val exported: Boolean, value: LangExpression) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    if (const) {
      if (pointer) throw new IllegalStateException("Constants with pointers are not allowed: " + name)
      if (exported) throw new IllegalStateException("Constants can not be exported: " + name)
      val cc = value.constantExpression(runtime).getOrElse(throw new IllegalStateException("const value is not a compile time constant: " + value.getClass.getSimpleName))
      runtime.createConstant(name, cc)
      (Nil, Nil)
    } else if (exported) {
      val data = value match {
        case LiteralNull =>
          runtime.checkType("variable creation", pointer, actual = true)
          DataInts((LiteralNull.plainResult, null))
        case _ =>
          runtime.checkType("variable creation", pointer, actual = false)
          value.constantExpression(runtime)
          .map(x => DataInts((x, null)))
          .getOrElse(throw new IllegalStateException("Exported values must have be compile time constant, an array literal or a null initializer: " + value.getClass.getSimpleName))
      }
      val entry = runtime.newDataEntry("var_" + name)
      val v = new Variable(name, MemoryData(entry), pointer, false)
      v.noConst()
      runtime.addGlobalVariable(v)
      (Nil, List(
        FieldLabel(FieldEntry(name, pointer)),
        DataBySizeValue(entry, 1, data)
      ))
    } else {
      runtime.startExpressionSection()
      val (code, data) = value.code(imports, runtime)
      runtime.endExpressionSection()
      val v = runtime.createVariable(name, pointer)
      if (value.constantExpression(runtime).isEmpty) {
        v.noConst()
      }
      runtime.checkType("variable creation", pointer, value.pointer())
      (code.appended(StmtMov(value.value(runtime), v.location)), data)
    }
  }
}
