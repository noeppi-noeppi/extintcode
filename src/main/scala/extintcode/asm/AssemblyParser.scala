package extintcode.asm

import extintcode.util.{CommonParsers, FieldEntry, FunctionEntry, InvalidFileException}
import org.apache.commons.text.StringEscapeUtils

import java.io.StringReader
import scala.util.matching.Regex

object AssemblyParser extends CommonParsers {
  
  val NL: Regex = "\\s*?\r?\n\\s*?".r
  val MODULE_NAME: Regex = "[A-Za-z][A-Za-z_0-9]*".r
  
  def parseTextLine(line: String): Option[Either[AssemblerStatement, AssemblerLabel]] = parseLine(line, text_line)
  def parseDataLine(line: String): Option[Either[AssemblerDataDefinition, AssemblerLabel]] = parseLine(line, data_line)
  
  private def parseLine[T](line: String, parser: Parser[T]): Option[T] = {
    if (line.trim.startsWith(";")) {
      None
    } else {
      Some(parseAll(parser, new StringReader(line)) match {
        case Success(x, _) => x
        case x: NoSuccess => System.err.println(x); throw new InvalidFileException("The parser returned an error")
      })
    }
  }
  
  type IntType = (Long, String)
  
  def text_line: Parser[Either[AssemblerStatement, AssemblerLabel]] = (statement | label_text) <~ comment ^^ {
    case x: AssemblerStatement => Left(x)
    case x: AssemblerLabel => Right(x)
  }
  
  def data_line: Parser[Either[AssemblerDataDefinition, AssemblerLabel]] = (datadef | label_field) <~ comment ^^ {
    case x: AssemblerDataDefinition => Left(x)
    case x: AssemblerLabel => Right(x)
  }

  def int: Parser[IntType] = int_rel_self | int_rel | int_plain
  def int_plain: Parser[IntType] = wholeNumber ^^ (x => (x.toLong, null))
  def int_rel: Parser[IntType] = MODULE_NAME ~ "%" ~ wholeNumber ^^ { case name ~ _ ~ num => (num.toLong, name) }
  def int_rel_self: Parser[IntType] = "%" ~> wholeNumber ^^ (x => (x.toLong, ""))
  
  def comment: Parser[Unit] = ("\\s*;".r ~ ".*".r | "\\s*".r) ^^ (_ => ())
  
  def ivalue: AssemblyParser.Parser[ValType] = special_address | direct_label | direct_data | ovalue | direct
  def ovalue: AssemblyParser.Parser[ValType] = special | memory_label | memory_data | memory_stack | memory
  
  def direct: Parser[ValType] = int ^^ (x => Direct(x._1, x._2))
  def memory: Parser[ValType] = "[" ~> int <~ "]" ^^ (x => Memory(x._1, x._2))
  def memory_stack: Parser[ValType] = "[" ~> "*" ~> int <~ "]" ^^ (x => MemoryStack(x._1, x._2))
  def direct_label: Parser[ValType] = "&" ~> MODULE_NAME ^^ (x => DirectLabel(x))
  def memory_label: Parser[ValType] = "[" ~> "&" ~> MODULE_NAME <~ "]" ^^ (x => MemoryLabel(x))
  def direct_data: Parser[ValType] = "!" ~> MODULE_NAME ^^ (x => DirectData(x))
  def memory_data: Parser[ValType] = "[" ~> "!" ~> MODULE_NAME <~ "]" ^^ (x => MemoryData(x))
  def special: Parser[ValType] = ("NEXTDYN" | "CALLSTACK" | "BACKJUMP" | "RETURN" | "GLOBAL[1-8]".r | "PARAM\\d+".r) ^^ (x => SpecialValue(x))
  def special_address: Parser[ValType] = "\\" ~> ("NEXTDYN" | "CALLSTACK" | "BACKJUMP" | "RETURN" | "GLOBAL[1-8]".r | "PARAM\\d+".r) ^^ (x => SpecialValueAddress(x))
  
  def statement: AssemblyParser.Parser[AssemblerStatement] = stmt_add | stmt_mul | stmt_inp | stmt_outp | stmt_jnz | stmt_jz | stmt_lt | stmt_eq | stmt_rel | stmt_ret | stmt_mov | stmt_jmp | stmt_push | stmt_pop | stmt_dyn | stmt_load | stmt_store | stmt_call | stmt_raw
  def stmt_add: Parser[AssemblerStatement] = "add" ~> ovalue ~ "," ~ ivalue ~ "," ~ ivalue ^^ { case out ~ _ ~ in1 ~ _ ~ in2 => StmtAdd(in1, in2, out) }
  def stmt_mul: Parser[AssemblerStatement] = "mul" ~> ovalue ~ "," ~ ivalue ~ "," ~ ivalue ^^ { case out ~ _ ~ in1 ~ _ ~ in2 => StmtMul(in1, in2, out) }
  def stmt_inp: Parser[AssemblerStatement] = "inp" ~> ovalue ^^ (x => StmtInp(x))
  def stmt_outp: Parser[AssemblerStatement] = "outp" ~> ivalue ^^ (x => StmtOutp(x))
  def stmt_jnz: Parser[AssemblerStatement] = "jnz" ~> ivalue ~ "," ~ ivalue ^^ { case target ~ _ ~ in => StmtJnz(in, target) }
  def stmt_jz: Parser[AssemblerStatement] = "jz" ~> ivalue ~ "," ~ ivalue ^^ { case target ~ _ ~ in => StmtJz(in, target) }
  def stmt_lt: Parser[AssemblerStatement] = "lt" ~> ovalue ~ "," ~ ivalue ~ "," ~ ivalue ^^ { case out ~ _ ~ in1 ~ _ ~ in2 => StmtLt(in1, in2, out) }
  def stmt_eq: Parser[AssemblerStatement] = "eq" ~> ovalue ~ "," ~ ivalue ~ "," ~ ivalue ^^ { case out ~ _ ~ in1 ~ _ ~ in2 => StmtEq(in1, in2, out) }
  def stmt_rel: Parser[AssemblerStatement] = "rel" ~> ivalue ^^ (x => StmtRel(x))
  def stmt_ret: Parser[AssemblerStatement] = "ret" ^^ (_ => StmtRet)
  def stmt_mov: Parser[AssemblerStatement] = "mov" ~> ovalue ~ "," ~ ivalue ^^ { case out ~ _ ~ in => StmtMov(in, out) }
  def stmt_jmp: Parser[AssemblerStatement] = "jmp" ~> ivalue ^^ (x => StmtJmp(x))
  def stmt_push: Parser[AssemblerStatement] = "push" ~> ivalue ^^ (x => StmtPush(x))
  def stmt_pop: Parser[AssemblerStatement] = "pop" ~> ivalue ^^ (x => StmtPop(x))
  def stmt_dyn: Parser[AssemblerStatement] = "dyn" ~> ovalue ~ "," ~ ivalue ^^ { case out ~ _ ~ amount => StmtDyn(amount, out) }
  def stmt_load: Parser[AssemblerStatement] = "load" ~> ovalue ~ "," ~ ivalue ^^ { case out ~ _ ~ pointer => StmtLoad(pointer, out) }
  def stmt_store: Parser[AssemblerStatement] = "store" ~> ivalue ~ "," ~ ivalue ^^ { case pointer ~ _ ~ in => StmtStore(in, pointer) }
  def stmt_call: Parser[AssemblerStatement] = "call" ~> ivalue ^^ (x => StmtCall(x))
  def stmt_raw: Parser[AssemblerStatement] = "raw" ~> rep1sep(int, ",") ^^ (x => StmtRaw(x))
  
  def datadef: AssemblyParser.Parser[AssemblerDataDefinition] = datadef_size_value | datadef_size | datadef_value
  def datadef_size: Parser[AssemblerDataDefinition] = MODULE_NAME ~ "(" ~ wholeNumber <~ ")" ^^ { case name  ~ _ ~ size => DataBySize(name, size.toInt) }
  def datadef_size_value: Parser[AssemblerDataDefinition] = MODULE_NAME ~ "(" ~ wholeNumber ~ ")" ~ data ^^ { case name  ~ _ ~ size ~ _ ~ data => DataBySizeValue(name, size.toInt, data) }
  def datadef_value: Parser[AssemblerDataDefinition] = MODULE_NAME ~ data ^^ { case name ~ data => DataByValue(name, data) }
  
  def data: Parser[AssemblerData] = rep1sep(data_no_rep, "|") ^^ (x => DataJoined(x: _*))
  def data_no_rep: Parser[AssemblerData] = data_ints | data_int_array | data_string_raw | data_string
  def data_ints: Parser[AssemblerData] = rep1sep(int, ",") ^^ (x => DataInts(x: _*))
  def data_int_array: Parser[AssemblerData] = "{" ~> repsep(int, ",") <~ "}" ^^ (x => DataIntArray(x: _*))
  def data_string_raw: Parser[AssemblerData] = "r" ~> escapedStringLiteral ^^ (x => DataInts(IntCodeAssembler.stringToCodePoints(x).map((_, null)): _*))
  def data_string: Parser[AssemblerData] = escapedStringLiteral ^^ (x => DataIntArray(IntCodeAssembler.stringToCodePoints(x).map((_, null)): _*))
  
  def label_text: Parser[AssemblerLabel] = label_func | label_code_func | label_code 
  def label_code: Parser[AssemblerLabel] = ":" ~> MODULE_NAME ^^ (x => CodeLabel(x))
  def label_code_func: Parser[AssemblerLabel] = ":" ~> MODULE_NAME ~ "#" ~ """(\??)(&?)\s*([A-Za-z][A-Za-z_0-9]*)\s*\(([&.]*)\)""".r ^^ { case name ~ _ ~ sig => CodeFunctionLabel(name, FunctionEntry.parse(sig)) }
  def label_func: Parser[AssemblerLabel] = ":\\s*#".r ~> """(\??)(&?)\s*([A-Za-z][A-Za-z_0-9]*)\s*\(([&.]*)\)""".r ^^ (x => FunctionLabel(FunctionEntry.parse(x)))
  def label_field: Parser[AssemblerLabel] = ":\\s*#".r ~> """(&?)\s*([A-Za-z][A-Za-z_0-9]*)""".r ^^ (x => FieldLabel(FieldEntry.parse(x)))
}
