package extintcode.compile.operator

import extintcode.compile.{LangExpression, Operator, Priority}

object Operators {
  
  val POWER: Operator[LangExpression] = new OperatorIml("^", Priority.POWER, new OperatorPow(_, _))
  val MULTIPLICATION: Operator[LangExpression] = new OperatorIml("*", Priority.MULTIPLICATIVE, new OperatorMul(_, _))
  val DIVISION: Operator[LangExpression] = new OperatorIml("/", Priority.MULTIPLICATIVE, new OperatorDiv(_, _))
  val MODULUS: Operator[LangExpression] = new OperatorIml("%", Priority.MULTIPLICATIVE, new OperatorMod(_, _))
  val ADDITION: Operator[LangExpression] = new OperatorIml("+", Priority.ADDITIVE, new OperatorAdd(_, _))
  val SUBTRACTION: Operator[LangExpression] = new OperatorIml("-", Priority.ADDITIVE, new OperatorSub(_, _))
  val LOWER: Operator[LangExpression] = new OperatorIml("<", Priority.RELATIONAL, new OperatorLt(_, _))
  val GREATER: Operator[LangExpression] = new OperatorIml(">", Priority.RELATIONAL, new OperatorGt(_, _))
  val LOWER_EQUAL: Operator[LangExpression] = new OperatorIml("<=", Priority.RELATIONAL, new OperatorLe(_, _))
  val GREATER_EQUAL: Operator[LangExpression] = new OperatorIml(">=", Priority.RELATIONAL, new OperatorGe(_, _))
  val EQUALS: Operator[LangExpression] = new OperatorIml("==", Priority.EQUALITY, new OperatorEq(_, _))
  val NOT_EQUALS: Operator[LangExpression] = new OperatorIml("!=", Priority.EQUALITY, new OperatorNeq(_, _))
  val LOGICAL_AND: Operator[LangExpression] = new OperatorIml("&&", Priority.LOGICAL_AND, new OperatorAnd(_, _))
  val LOGICAL_OR: Operator[LangExpression] = new OperatorIml("||", Priority.LOGICAL_OR, new OperatorOr(_, _))
  
  // Operators with names which are part of other operators must be located after those
  val OPS: List[Operator[LangExpression]] = List(
    POWER,
    MULTIPLICATION,
    DIVISION,
    MODULUS,
    ADDITION,
    SUBTRACTION,
    LOWER_EQUAL,
    GREATER_EQUAL,
    LOWER,
    GREATER,
    EQUALS,
    NOT_EQUALS,
    LOGICAL_AND,
    LOGICAL_OR
  )
  
  class OperatorIml(name: String, priority: Priority, join: (LangExpression, LangExpression) => LangExpression) extends Operator[LangExpression](name, priority) {
    override def apply(in1: LangExpression, in2: LangExpression): LangExpression = join(in1, in2)
  }
}
