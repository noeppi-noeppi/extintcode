package extintcode.asm

import org.apache.commons.text.StringEscapeUtils

sealed trait AssemblerData {
  val size: Int
  def code(ref: Map[String, Int]): Seq[(Long, String)]
  def string(): String
}

case class DataJoined(parents: AssemblerData*) extends AssemblerData {
  override val size: Int = parents.map(_.size).sum
  override def code(ref: Map[String, Int]): Seq[(Long, String)] = parents.flatMap(_.code(ref))
  def string(): String = parents.mkString(" | ")
}

case class DataInts(ints: (Long, String)*) extends AssemblerData {
  override val size: Int = ints.size
  override def code(ref: Map[String, Int]): Seq[(Long, String)] = ints
  def string(): String = ints.map(x => Direct(x._1, x._2)).map(_.string()).mkString(", ")
}

case class DataIntArray(ints: (Long, String)*) extends AssemblerData {
  override val size: Int =  1 + ints.size
  override def code(ref: Map[String, Int]): Seq[(Long, String)] = ints.prepended((ints.size, null))
  def string(): String = ints.map(x => Direct(x._1, x._2)).map(_.string()).mkString("{ ", ", ", " }")
}

case class DataRawString(str: String) extends AssemblerData {
  private val ints = IntCodeAssembler.stringToCodePoints(str).map((_, null))
  override val size: Int = ints.size
  override def code(ref: Map[String, Int]): Seq[(Long, String)] = ints
  def string(): String = "r\"" + StringEscapeUtils.escapeJava(str) + "\""
}

case class DataString(str: String) extends AssemblerData {
  private val ints = IntCodeAssembler.stringToCodePoints(str).map((_, null))
  override val size: Int =  1 + ints.size
  override def code(ref: Map[String, Int]): Seq[(Long, String)] = ints.prepended((ints.size, null))
  def string(): String = "\"" + StringEscapeUtils.escapeJava(str) + "\""
}

case class DataReference(data: String) extends AssemblerData {
  override val size: Int = 1
  override def code(ref: Map[String, Int]): Seq[(Long, String)] = Seq((ref(data), ""))
  override def string(): String = "!" + data
}