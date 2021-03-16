package extintcode.compile

import extintcode.compile.array.{ArrayAccess, ArrayBySize, ArrayUpdate}
import extintcode.compile.control.{CodeBlock, ControlIf, ControlJumpStatement, ControlJumpType, ControlWhile}
import extintcode.compile.function.{FunctionCallExpression, FunctionCallStatement, FunctionDefinition, ReturnStatement}
import extintcode.compile.literal.{LiteralArray, LiteralInt, LiteralNull, LiteralString}
import extintcode.compile.meta.{ImportStatement, TypeCast}
import extintcode.compile.operator.Operators
import extintcode.compile.unary.{LogicalNot, Negation}
import extintcode.compile.variable.{CreatePointer, VariableAccess, VariableDeclaration, VariableDeref, VariableUpdate}
import extintcode.util.InvalidFileException

import java.io.Reader
import scala.util.matching.Regex

object ProgramParser extends ExtendedParsers {
  
  val MODULE_NAME: Regex = "[A-Za-z][A-Za-z_0-9]*".r
  val IMPORT_NAME: Regex = "[A-Za-z][A-Za-z_0-9]*(\\.(\\*|[A-Za-z][A-Za-z_0-9]*))?".r
  
  def parseProgram(in: Reader): List[Either[LangStatement, FunctionDefinition]] = parseAll(program, in) match {
    case Success(x, _) => x
    case x: NoSuccess => System.err.println(x); throw new InvalidFileException("The parser returned an error")
  }
  
  def program: Parser[List[Either[LangStatement, FunctionDefinition]]] = rep(lang)
  def lang: Parser[Either[LangStatement, FunctionDefinition]] = (statement ^^ (x => Left(x))) | (function_definition ^^ (x => Right(x)))
  
  def statement: Parser[LangStatement] = (raw_statement <~ ";") | control
  def raw_statement: Parser[LangStatement] = name_import | implicit_import | function_statement | return_statement | control_break | control_continue | control_next | variable_declaration | array_paren_update | array_variable_update | variable_update
  def control: Parser[LangStatement] = control_if_else | control_if | control_while | control_block
  
  def expression: Parser[LangExpression] = op1sep(raw_expression, Operators.OPS)
  def raw_expression: Parser[LangExpression] = literal | function_expression | cast_direct | cast_pointer | array_by_size | create_pointer | logical_not | unary_negation | array_paren_access | variable_deref | array_variable_access | variable_access
  
  def keyword: Parser[String] = "import" | "implicit" | "true" | "false" | "null" | "array" | "export" | "let" | "const" | "as" | "def" | "return" | "break" | "continue" | "next" | "if" | "else" | "while"
  
  def identifier: Parser[String] = MODULE_NAME - keyword
  def fqn: Parser[(Option[String], String)] = ((identifier ~ "." ~ identifier) ^^ { case module ~ _ ~ name => (Some(module), name) }) | (identifier ^^ (x => (None, x)))
  
  def name_import: Parser[LangStatement] = "import" ~> IMPORT_NAME ^^ (x => new ImportStatement(x, false))
  def implicit_import: Parser[LangStatement] = "implicit" ~> IMPORT_NAME ^^ (x => new ImportStatement(x, true))
  
  def literal: Parser[LangExpression] = literal_int | literal_true | literal_false | literal_null | literal_array | literal_string
  def literal_int: Parser[LangExpression] = wholeNumber ^^ (x => new LiteralInt(x.toLong))
  def literal_true: Parser[LangExpression] = "true" ^^ (_ => new LiteralInt(1))
  def literal_false: Parser[LangExpression] = "false" ^^ (_ => new LiteralInt(0))
  def literal_null: Parser[LangExpression] = "null" ^^ (_ => LiteralNull)
  def literal_array: Parser[LangExpression] = "array" ~> "{" ~> repsep(wholeNumber, ",") <~ "}" ^^ (x => new LiteralArray(x.map(_.toLong)))
  def literal_string: Parser[LangExpression] = escapedStringLiteral ^^ (x => new LiteralString(x))
  
  def unary_negation: Parser[LangExpression] = "-" ~> expression ^^ (x => new Negation(x))
  def logical_not: Parser[LangExpression] = "!" ~> expression ^^ (x => new LogicalNot(x))
  
  def variable_declaration: Parser[LangStatement] = opt("export") ~ ("let" | "const") ~ opt("&") ~ identifier ~ "=" ~ expression ^^ { case export ~ key ~ mode ~ name ~ _ ~ value => new VariableDeclaration(name, mode.isDefined, key == "const", export.isDefined, value) }
  def variable_deref: Parser[LangExpression] = "*" ~> identifier ^^ (x => new VariableDeref(x))
  def variable_access: Parser[LangExpression] = identifier ^^ (x => new VariableAccess(x))
  def variable_update: Parser[LangStatement] = identifier ~ "=" ~ expression ^^ { case name ~ _ ~ value => new VariableUpdate(name, value) }
  
  def cast_direct: Parser[LangExpression] = "as" ~> "[" ~> "direct" ~> "]" ~> "(" ~> expression <~ ")" ^^ (x => new TypeCast(x, false))
  def cast_pointer: Parser[LangExpression] = "as" ~> "[" ~> "pointer" ~> "]" ~> "(" ~> expression <~ ")" ^^ (x => new TypeCast(x, true))
  def create_pointer: Parser[LangExpression] = "&" ~> expression ^^ (x => new CreatePointer(x))
  
  def function_statement: Parser[LangStatement] = fqn ~ "(" ~ repsep(expression, ",") <~ ")" ^^ { case name ~ _ ~ args => new FunctionCallStatement(name._1, name._2, args) }
  def function_expression: Parser[LangExpression] = fqn ~ "(" ~ repsep(expression, ",") <~ ")" ^^ { case name ~ _ ~ args => new FunctionCallExpression(name._1, name._2, args) }
  def function_param: Parser[(String, Boolean)] = opt("&") ~ identifier ^^ { case mode ~ name => (name, mode.isDefined) }
  def function_definition: Parser[FunctionDefinition] = "def" ~> opt("&") ~ identifier ~ "(" ~ repsep(function_param, ",") ~ ")" ~ "{" ~ rep(statement) <~ "}" ^^ { case mode ~ name ~ _ ~ params ~ _ ~ _ ~ content => new FunctionDefinition(name, mode.isDefined, params, content) }
  def return_statement: Parser[LangStatement] = "return" ~> expression ^^ (x => new ReturnStatement(x))

  def array_by_size: Parser[LangExpression] = "array" ~> "[" ~> expression <~ "]" ^^ (x => new ArrayBySize(x))
  def array_variable_access: Parser[LangExpression] = identifier ~ "[" ~ expression <~ "]" ^^ { case array ~ _ ~ idx => new ArrayAccess(new VariableAccess(array), idx) }
  def array_paren_access: Parser[LangExpression] = "(" ~> expression ~ ")" ~ "[" ~ expression <~ "]" ^^ { case array ~ _ ~ _ ~ idx => new ArrayAccess(array, idx) }
  def array_variable_update: Parser[LangStatement] = identifier ~ "[" ~ expression ~ "]" ~ "=" ~ expression ^^ { case array ~ _ ~ idx ~ _ ~ _ ~ value => new ArrayUpdate(new VariableAccess(array), idx, value) }
  def array_paren_update: Parser[LangStatement] = "(" ~> expression ~ ")" ~ "[" ~ expression ~ "]" ~ "=" ~ expression ^^ { case array ~ _ ~ _ ~ idx ~ _ ~ _ ~ value => new ArrayUpdate(array, idx, value) }
  
  def control_if: Parser[LangStatement] = "if" ~> "(" ~> expression ~ ")" ~ "{" ~ rep(statement) <~ "}" ^^ { case condition ~ _  ~ _ ~ ifTrue => new ControlIf(condition, ifTrue, Nil) }
  def control_if_else: Parser[LangStatement] = "if" ~> "(" ~> expression ~ ")" ~ "{" ~ rep(statement) ~ "}" ~ "else" ~ "{" ~ rep(statement) <~ "}" ^^ { case condition ~ _  ~ _ ~ ifTrue ~ _ ~ _ ~ _ ~ ifFalse => new ControlIf(condition, ifTrue, ifFalse) }
  def control_while: Parser[LangStatement] = "while" ~> "(" ~> expression ~ ")" ~ "{" ~ rep(statement) <~ "}" ^^ { case condition ~ _  ~ _ ~ statements => new ControlWhile(condition, statements) }
  def control_block: Parser[LangStatement] = "{" ~> rep(statement) <~ "}" ^^ (x => new CodeBlock(x))
  def control_break: Parser[LangStatement] = "break" ^^ (_ => new ControlJumpStatement(ControlJumpType.BREAK))
  def control_continue: Parser[LangStatement] = "continue" ^^ (_ => new ControlJumpStatement(ControlJumpType.CONTINUE))
  def control_next: Parser[LangStatement] = "next" ^^ (_ => new ControlJumpStatement(ControlJumpType.NEXT))
}
