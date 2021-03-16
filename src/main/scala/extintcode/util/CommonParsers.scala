package extintcode.util

import org.apache.commons.text.StringEscapeUtils

import scala.util.parsing.combinator.JavaTokenParsers

class CommonParsers extends JavaTokenParsers {

  def escapedStringLiteral: Parser[String] = stringLiteral ^^ (x => unquote(x))

  private def unquote(quoted: String): String = {
    val raw = if ((quoted.startsWith("\"") && quoted.endsWith("\"")) || (quoted.startsWith("\'") && quoted.endsWith("\'"))) {
      quoted.substring(1, quoted.length - 1)
    } else {
      quoted
    }
    StringEscapeUtils.unescapeJava(raw)
  }
}
