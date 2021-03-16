package extintcode.util

import scala.util.matching.Regex

case class FunctionEntry(name: String, pure: Boolean, pointerReturn: Boolean, signature: List[Boolean]) {
  
  val args: Int = signature.size
  
  override def toString: String = (if (pure) "?" else "") + (if (pointerReturn) "&" else "") + name + "(" + signature.map(if(_) "&" else ".").mkString("") + ")"
  def toStringWithAddress(address: Long): String = toString + "+" + address.toString
}

object FunctionEntry {
  
  val REGEX: Regex = """^\s*(\??)\s*(&?)\s*([A-Za-z][A-Za-z_0-9]*)\s*\(([&.]*)\)\s*$""".r
  val REGEX_WA: Regex = """^\s*(\??)\s*(&?)\s*([A-Za-z][A-Za-z_0-9]*)\s*\(([&.]*)\)\s*\+\s*(\d+)\s*$""".r
  
  def parse(string: String): FunctionEntry = string match {
    case REGEX(p, r, n, s) => FunctionEntry(n, p == "?", r == "&", s.toCharArray.map(_ == '&').toList)
    case _ => throw new InvalidFileException("Invalid method signature: " + string)
  }
  
  def parseWithAddress(string: String): (FunctionEntry, Long) = string match {
    case REGEX_WA(p, r, n, s, a) => (FunctionEntry(n, p == "?", r == "&", s.toCharArray.map(_ == '&').toList), a.toLong)
    case _ => throw new InvalidFileException("Invalid method signature: " + string)
  }
}
