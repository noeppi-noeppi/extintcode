package extintcode.asm

import extintcode.util.{FieldEntry, FunctionEntry}

sealed trait AssemblerLabel {
  
  val name: Option[String]
  val field: Option[FieldEntry]
  val function: Option[FunctionEntry]
}

case class CodeLabel(labelName: String) extends AssemblerLabel with AssemblyText {
  override val name: Option[String] = Some(labelName)
  override val field: Option[FieldEntry] = None
  override val function: Option[FunctionEntry] = None
  override def textStr(): String = ":" + labelName
}

case class CodeFunctionLabel(labelName: String, functionEntry: FunctionEntry) extends AssemblerLabel with AssemblyText {
  override val name: Option[String] = Some(labelName)
  override val field: Option[FieldEntry] = None
  override val function: Option[FunctionEntry] = Some(functionEntry)
  override def textStr(): String = ":" + labelName + "#" + functionEntry.toString
}

case class FunctionLabel(functionEntry: FunctionEntry) extends AssemblerLabel with AssemblyText {
  override val name: Option[String] = None
  override val field: Option[FieldEntry] = None
  override val function: Option[FunctionEntry] = Some(functionEntry)
  override def textStr(): String = ":#" + functionEntry.toString
}

case class FieldLabel(fieldEntry: FieldEntry) extends AssemblerLabel with AssemblyData {
  override val name: Option[String] = None
  override val field: Option[FieldEntry] = Some(fieldEntry)
  override val function: Option[FunctionEntry] = None
  override def dataStr(): String = ":#" + fieldEntry.toString
}
