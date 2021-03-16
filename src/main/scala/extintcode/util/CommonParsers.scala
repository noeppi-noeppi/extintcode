package extintcode.util

import org.apache.commons.text.StringEscapeUtils

import scala.util.parsing.combinator.JavaTokenParsers

class CommonParsers extends JavaTokenParsers {

  def escapedStringLiteral: Parser[String] = stringLiteral ^^ (x => unquote(x))
  def stringLiteralSingleQuotes: Parser[String] = ("\'"+"""([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\'").r
  def escapedStringLiteralSingleQuotes: Parser[String] = stringLiteralSingleQuotes ^^ (x => unquoteSingle(x))

  private def unquote(quoted: String): String = {
    val raw = if (quoted.startsWith("\"") && quoted.endsWith("\"")) {
      quoted.substring(1, quoted.length - 1)
    } else {
      quoted
    }
    StringEscapeUtils.unescapeJava(raw)
  }
  
  private def unquoteSingle(quoted: String): String = {
    val raw = if (quoted.startsWith("\'") && quoted.endsWith("\'")) {
      quoted.substring(1, quoted.length - 1)
    } else {
      quoted
    }
    StringEscapeUtils.unescapeJava(raw)
  }
}
