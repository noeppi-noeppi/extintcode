package extintcode.asm

import extintcode.util.InvalidFileException

sealed trait AssemblerDataDefinition extends AssemblyData {
  
  val name: String
  val size: Int
  def code(ref: Map[String, Int]): Seq[(Long, String)]
}

case class DataBySize(override val name: String, override val size: Int) extends AssemblerDataDefinition {
  override def code(ref: Map[String, Int]): Seq[(Long, String)] = Seq.fill(size)((0, null))
  override def dataStr(): String = name + "(" + size + ")"
}

case class DataByValue(override val name: String, data: AssemblerData) extends AssemblerDataDefinition {
  override val size: Int = data.size
  override def code(ref: Map[String, Int]): Seq[(Long, String)] = data.code(ref)
  override def dataStr(): String = name + " ".repeat(Math.max(1, 16 - name.length)) + data.string()
}

case class DataBySizeValue(override val name: String, override val size: Int, data: AssemblerData) extends AssemblerDataDefinition {
  
  if (data.size > size) throw new InvalidFileException("Data definition exceeds maximum size: " + data.size + " (" + size + ")")
  
  override def code(ref: Map[String, Int]): Seq[(Long, String)] = data.code(ref).appendedAll(Seq.fill(size - data.size)((0, null)))
  override def dataStr(): String = {
    val definition = name + "(" + size + ")"
    definition + " ".repeat(Math.max(1, 16 - definition.length)) + data.string()
  }
}
