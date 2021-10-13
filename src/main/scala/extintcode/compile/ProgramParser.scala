package extintcode.compile

import extintcode.compile.array.{ArrayAccess, ArrayBySize, ArrayUpdate}
import extintcode.compile.control.{CodeBlock, ControlFor, ControlForeach, ControlIf, ControlJumpStatement, ControlJumpType, ControlWhile}
import extintcode.compile.function.{FunctionCallExpression, FunctionCallStatement, FunctionDefinition, ReturnStatement}
import extintcode.compile.literal.{LiteralArray, LiteralBool, LiteralChar, LiteralInt, LiteralNull, LiteralString, LiteralVoid}
import extintcode.compile.meta.{ImportStatement, TypeCast}
import extintcode.compile.operator.{OperatorAdd, OperatorDiv, OperatorMul, OperatorSub, Operators}
import extintcode.compile.operator2.{LogicalNot, Negation, Ternary}
import extintcode.compile.variable.{CreatePointer, VariableAccess, VariableDeclaration, VariableDeref, VariableUpdate}
import extintcode.util.InvalidFileException
import org.apache.commons.io.input.SequenceReader

import java.io.{Reader, StringReader}
import scala.util.matching.Regex

object ProgramParser extends ExtendedParsers {
  
  val MODULE_NAME: Regex = "[A-Za-z][A-Za-z_0-9]*".r
  val IMPORT_NAME: Regex = "[A-Za-z][A-Za-z_0-9]*(\\.(\\*|[A-Za-z][A-Za-z_0-9]*))?".r
  
  // Additional line breaks are required for comments to match.
  def parseProgram(in: Reader): List[Either[LangStatement, FunctionDefinition]] = parseAll(program, new SequenceReader(in, new StringReader("\n\n"))) match {
    case Success(x, _) => x
    case x: NoSuccess => System.err.println(x); throw new InvalidFileException("The parser returned an error")
  }

  // Comments
  override protected val whiteSpace: Regex = "\\s*((#.*\r?\n)?\\s*)*".r
  
  def program: Parser[List[Either[LangStatement, FunctionDefinition]]] = rep(lang)
  def lang: Parser[Either[LangStatement, FunctionDefinition]] = (statement ^^ (x => Left(x))) | (function_definition ^^ (x => Right(x)))
  
  def statement: Parser[LangStatement] = (raw_statement <~ ";") | control
  def raw_statement: Parser[LangStatement] = name_import | implicit_import | function_statement | return_statement_any | control_break | control_continue | control_next | variable_declaration | array_paren_update | array_variable_update | variable_update | assign_add | assign_sub | assign_mul | assign_div
  def control: Parser[LangStatement] = control_if_else | control_if | control_while | control_foreach | control_for | control_block
  
  def expression: Parser[LangExpression] = op1sep(raw_expression, Operators.OPS)
  def raw_expression: Parser[LangExpression] = ternary | raw_expression_nt
  
  // If ternary was added here we had left recursion.
  def expression_nt: Parser[LangExpression] = op1sep(raw_expression_nt, Operators.OPS)
  def raw_expression_nt: Parser[LangExpression] = literal | function_expression | cast_direct | cast_pointer | array_by_size | create_pointer | logical_not | unary_negation | array_paren_access | variable_deref | array_variable_access | variable_access
  
  def keyword: Parser[String] = "import" | "implicit" | "true" | "false" | "null" | "array" | "export" | "let" | "const" | "as" | "def" | "return" | "break" | "continue" | "next" | "if" | "else" | "while"
  
  def identifier: Parser[String] = MODULE_NAME - keyword
  def fqn: Parser[(Option[String], String)] = ((identifier ~ "." ~ identifier) ^^ { case module ~ _ ~ name => (Some(module), name) }) | (identifier ^^ (x => (None, x)))
  
  def name_import: Parser[LangStatement] = "import" ~> IMPORT_NAME ^^ (x => new ImportStatement(x, false))
  def implicit_import: Parser[LangStatement] = "implicit" ~> IMPORT_NAME ^^ (x => new ImportStatement(x, true))
  
  def literal: Parser[LangExpression] = literal_int | literal_true | literal_false | literal_null | literal_array | literal_string | literal_char
  def literal_int: Parser[LangExpression] = wholeNumber ^^ (x => new LiteralInt(x.toLong))
  def literal_char: Parser[LangExpression] = escapedStringLiteralSingleQuotes ^^ (x => new LiteralChar(x))
  def literal_true: Parser[LangExpression] = "true" ^^ (_ => new LiteralBool(true))
  def literal_false: Parser[LangExpression] = "false" ^^ (_ => new LiteralBool(false))
  def literal_null: Parser[LangExpression] = "null" ^^ (_ => LiteralNull)
  def literal_void: Parser[LangExpression] = "_" ^^ (_ => LiteralVoid)
  def literal_array: Parser[LangExpression] = "array" ~> "{" ~> repsep(wholeNumber, ",") <~ "}" ^^ (x => new LiteralArray(x.map(_.toLong)))
  def literal_string: Parser[LangExpression] = escapedStringLiteral ^^ (x => new LiteralString(x))
  
  def unary_negation: Parser[LangExpression] = "-" ~> expression ^^ (x => new Negation(x))
  def logical_not: Parser[LangExpression] = "!" ~> expression ^^ (x => new LogicalNot(x))
  def ternary: Parser[LangExpression] = expression_nt ~ "?" ~ expression ~ ":" ~ expression ^^ { case condition ~ _ ~ ifTrue ~ _ ~ ifFalse => new Ternary(condition, ifTrue, ifFalse) }
  
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
  def return_statement_any: Parser[LangStatement] = return_statement | return_statement_void | failure("Return statement expected.")
  def return_statement: Parser[LangStatement] = "return" ~> expression ^^ (x => new ReturnStatement(x))
  def return_statement_void: Parser[LangStatement] = "return" ^^ (x => new ReturnStatement(LiteralVoid))

  def array_by_size: Parser[LangExpression] = "array" ~> "[" ~> expression <~ "]" ^^ (x => new ArrayBySize(x))
  def array_variable_access: Parser[LangExpression] = identifier ~ "[" ~ expression <~ "]" ^^ { case array ~ _ ~ idx => new ArrayAccess(new VariableAccess(array), idx) }
  def array_paren_access: Parser[LangExpression] = "(" ~> expression ~ ")" ~ "[" ~ expression <~ "]" ^^ { case array ~ _ ~ _ ~ idx => new ArrayAccess(array, idx) }
  def array_variable_update: Parser[LangStatement] = identifier ~ "[" ~ expression ~ "]" ~ "=" ~ expression ^^ { case array ~ _ ~ idx ~ _ ~ _ ~ value => new ArrayUpdate(new VariableAccess(array), idx, value) }
  def array_paren_update: Parser[LangStatement] = "(" ~> expression ~ ")" ~ "[" ~ expression ~ "]" ~ "=" ~ expression ^^ { case array ~ _ ~ _ ~ idx ~ _ ~ _ ~ value => new ArrayUpdate(array, idx, value) }
  
  def control_if: Parser[LangStatement] = "if" ~> "(" ~> expression ~ ")" ~ statement ^^ { case condition ~  _ ~ ifTrue => new ControlIf(condition, List(ifTrue), Nil) }
  def control_if_else: Parser[LangStatement] = "if" ~> "(" ~> expression ~ ")" ~ statement ~ "else" ~ statement ^^ { case condition ~ _ ~ ifTrue ~ _ ~ ifFalse => new ControlIf(condition, List(ifTrue), List(ifFalse)) }
  def control_while: Parser[LangStatement] = "while" ~> "(" ~> expression ~ ")" ~ statement ^^ { case condition ~ _ ~ statements => new ControlWhile(condition, List(statements)) }
  def control_for: Parser[LangStatement] = "for" ~> "(" ~> opt(raw_statement) ~ ";" ~ expression ~ ";" ~ opt(raw_statement) ~ ")" ~ statement ^^ { case init ~ _ ~ condition ~ _ ~ last ~ _ ~ statements => new ControlFor(init, condition, last, List(statements)) }
  def control_foreach: Parser[LangStatement] = "for" ~> "(" ~> opt("&") ~ identifier ~ opt("@" ~> identifier) ~ ":" ~ expression ~ ")" ~ statement ^^ { case varType ~ varName ~ indexVarName ~ _ ~ array ~ _ ~ statements => new ControlForeach(varName, varType.isDefined, indexVarName, array, List(statements)) }
  def control_block: Parser[LangStatement] = "{" ~> rep(statement) <~ "}" ^^ (x => new CodeBlock(x))
  def control_break: Parser[LangStatement] = "break" ^^ (_ => new ControlJumpStatement(ControlJumpType.BREAK))
  def control_continue: Parser[LangStatement] = "continue" ^^ (_ => new ControlJumpStatement(ControlJumpType.CONTINUE))
  def control_next: Parser[LangStatement] = "next" ^^ (_ => new ControlJumpStatement(ControlJumpType.NEXT))
  
  def assign_add: Parser[LangStatement] = identifier ~ "+=" ~ expression ^^ { case name ~ _ ~ value => new VariableUpdate(name, new OperatorAdd(new VariableAccess(name), value)) }
  def assign_sub: Parser[LangStatement] = identifier ~ "-=" ~ expression ^^ { case name ~ _ ~ value => new VariableUpdate(name, new OperatorSub(new VariableAccess(name), value)) }
  def assign_mul: Parser[LangStatement] = identifier ~ "*=" ~ expression ^^ { case name ~ _ ~ value => new VariableUpdate(name, new OperatorMul(new VariableAccess(name), value)) }
  def assign_div: Parser[LangStatement] = identifier ~ "/=" ~ expression ^^ { case name ~ _ ~ value => new VariableUpdate(name, new OperatorDiv(new VariableAccess(name), value)) }
}
