package extintcode.util

import scala.util.matching.Regex

case class FieldEntry(name: String, pointer: Boolean) {
  
  override def toString: String = (if (pointer) "&" else "") + name
  def toStringWithAddress(address: Long): String = toString + "+" + address.toString
}

object FieldEntry {
  
  val REGEX: Regex = """^\s*(&?)\s*([A-Za-z][A-Za-z_0-9]*)\s*$""".r
  val REGEX_WA: Regex = """^\s*(&?)\s*([A-Za-z][A-Za-z_0-9]*)\s*\+\s*(\d+)\s*$""".r
  
  def parse(string: String): FieldEntry = string match {
    case REGEX(s, n) => FieldEntry(n, s == "&")
    case _ => throw new InvalidFileException("Invalid field signature: " + string)
  }
  
  def parseWithAddress(string: String): (FieldEntry, Long) = string match {
    case REGEX_WA(s, n, a) => (FieldEntry(n, s == "&"), a.toLong)
    case _ => throw new InvalidFileException("Invalid field signature: " + string)
  }
}
