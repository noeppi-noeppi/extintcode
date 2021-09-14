package extintcode.compile.variable

import extintcode.asm._
import extintcode.compile.literal.{LiteralArray, LiteralNull}
import extintcode.compile._
import extintcode.compile.frame.GlobalFrame
import extintcode.util.FieldEntry

import scala.collection.mutable.ListBuffer

class VariableDeclaration(name: String, pointer: Boolean, val const: Boolean, val exported: Boolean, value: LangExpression) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    if (const) {
      if (pointer) throw new IllegalStateException("Constants with pointers are not allowed: " + name)
      if (exported) throw new IllegalStateException("Constants can not be exported: " + name)
      val cc = value.constantExpression(runtime).getOrElse(throw new IllegalStateException("const value is not a compile time constant: " + value.getClass.getSimpleName))
      runtime.createConstant(name, cc)
      (Nil, Nil)
    } else if (exported) {
      val (data, additional) = value match {
        case LiteralNull =>
          runtime.checkType("variable creation", pointer, actual = true)
          val d = DataInts((LiteralNull.plainResult, null))
          (d, Nil)
        case array: LiteralArray =>
          runtime.checkType("variable creation", pointer, actual = true)
          val ref = runtime.newDataEntry("var_" + name + "_init")
          val d = DataReference(ref)
          val a = DataByValue(ref, DataIntArray(array.values.map(x => (x, null)): _*))
          (d, List(a))
        case _ =>
          runtime.checkType("variable creation", pointer, actual = false)
          val d = value.constantExpression(runtime)
            .map(x => DataInts((x, null)))
            .getOrElse(throw new IllegalStateException("Exported values must have be compile time constant, an array literal or a null initializer: " + value.getClass.getSimpleName))
          (d, Nil)
      }
      val entry = runtime.newDataEntry("var_" + name)
      val v = new Variable(name, MemoryData(entry), pointer, false)
      v.noConst()
      runtime.addGlobalVariable(v)
      (Nil, List(
        GlobalFrame(name, v.location),
        FieldLabel(FieldEntry(name, pointer)),
        DataByValue(entry, data)
      ).appendedAll(additional))
    } else {
      val text = ListBuffer[AssemblyText]()
      text.addAll(runtime.startExpressionSection())
      val (t, data) = value.code(imports, runtime)
      text.addAll(t)
      text.addAll(runtime.endExpressionSection())
      val (v, frames) = runtime.createVariable(name, pointer)
      text.addAll(frames)
      if (value.constantExpression(runtime).isEmpty) {
        v.noConst()
      }
      runtime.checkType("variable creation", pointer, value.pointer())
      text.addOne(StmtMov(value.value(runtime), v.location))
      (text.toList, data)
    }
  }
}
