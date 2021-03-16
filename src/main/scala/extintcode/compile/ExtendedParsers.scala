package extintcode.compile

import extintcode.util.CommonParsers

class ExtendedParsers extends CommonParsers {

  def op1sep[T](expression: Parser[T], operators: Set[Operator[T]]): Parser[T] = {
    
    def isOp(priority: Priority)(elem: Either[T, Operator[T]]): Boolean = elem match {
      case Right(x) => x.priority == priority
      case _ => false
    }
    
    val operatorParser: Parser[Operator[T]] = operators
      .map(op => op.name ^^ (_ => op))
      .reduce((op1, op2) => op1 | op2)
    
    // Luckily this works as the aargument in `|` is by name
    var parser: Parser[T] = null
    
    val parenExpression = expression | ("(" ~> parser <~ ")")
    
    val operatorSeparatedListParser: Parser[List[Either[T, Operator[T]]]] =
      ((parenExpression ^^ (x => Left(x))) ~ rep(operatorParser ~ parenExpression ^^ { case op ~ expr => List(Right(op), Left(expr)) })) ^^ { case x ~ y => x::y.flatten }

    parser = operatorSeparatedListParser ^^ (list => {
      var join = list
      Priority.values().foreach(priority => {
        val check = isOp(priority) _
        while (join.exists(check)) {
          val idx = if (priority.rightAssociative) join.lastIndexWhere(check) else join.indexWhere(check)
          val p1 = join(idx - 1) match { case Left(x) => x; case _ => throw new IllegalStateException("Incorrect operator chaining: Joined expression: " + join) }
          val p2 = join(idx + 1) match { case Left(x) => x; case _ => throw new IllegalStateException("Incorrect operator chaining: Joined expression: " + join) }
          val result = join(idx) match { case Right(x) => x.apply(p1, p2); case _ => throw new IllegalStateException("Incorrect operator chaining: Joined expression: " + join) }
          join = join.take(idx - 1).appended(Left(result)).appendedAll(join.drop(idx + 2))
        }
      })
      if (join.isEmpty || join.size > 1) throw new IllegalStateException("Incorrect operator chaining: Joined expression: " + join)
      join.head match { case Left(x) => x; case _ => throw new IllegalStateException("Incorrect operator chaining: Joined expression: " + join) }
    })
    parser
  }
}
