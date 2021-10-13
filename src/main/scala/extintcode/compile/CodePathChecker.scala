package extintcode.compile

import extintcode.util.FunctionEntry

object CodePathChecker {

  def yes(): CodePathCheckResult = Yes
  
  def no(name: LangStatement): CodePathCheckResult = no(name.getClass.getSimpleName)
  def no(name: String): CodePathCheckResult = No(name)

  def never(name: LangStatement): CodePathCheckResult = never(name.getClass.getSimpleName)
  def never(name: String): CodePathCheckResult = Never(name)
  
  def child(name: LangStatement, stmt: LangStatement): CodePathCheckResult = child(name.getClass.getSimpleName, stmt)
  def children(name: LangStatement, stmts: Seq[LangStatement]): CodePathCheckResult = children(name.getClass.getSimpleName, stmts)
  
  def child(name: String, stmt: LangStatement): CodePathCheckResult = children(name, List(stmt))
  def children(name: String, stmts: Seq[LangStatement]): CodePathCheckResult = children(name, toplevel = false, stmts)

  def toplevel(func: FunctionEntry, stmts: Seq[LangStatement]): Unit = {
    val result = children(func.toString, toplevel = true, stmts)
    if (!result.success) {
      def path(result: CodePathCheckResult): String = result match {
        case No(name) => name
        case Never(name) => "!" + name
        case ChildNo(name, child) => name + " -> " + path(child)
        case _ => "?"
      }
      val failurePath = result match {
        case ChildNo(name, child) if name == func.toString => path(child)
        case result => path(result)
      }
      throw new IllegalStateException("For function " + func.toString + ": Not all code paths return a value: " + failurePath)
    }
  }
  
  private def children(name: String, toplevel: Boolean, stmts: Seq[LangStatement]): CodePathCheckResult = {
    var lastFail: CodePathCheckResult = No(name)
    for (stmt <- stmts) {
      stmt.checkCodePath() match {
        case result if result.success => return Yes
        case result if result.never && !toplevel => return ChildNo(name, result)
        case result => lastFail = ChildNo(name, result)
      }
    }
    lastFail
  }
  
  def and(results: CodePathCheckResult*): CodePathCheckResult = results.find(_.never).orElse(results.find(!_.success)).getOrElse(Yes)
  
  protected case object Yes extends CodePathCheckResult {
    override val success: Boolean = true
    override val never: Boolean = false
    override val cause: Option[CodePathCheckResult] = None
  }
  
  protected case class No(name: String) extends CodePathCheckResult {
    override val success: Boolean = false
    override val never: Boolean = false
    override val cause: Option[CodePathCheckResult] = None
  }
  
  // Something that jumps somewhere and thus code path checking can't apply.
  // So we can't be sure. A yes before a never is still a yes because the jump
  // never is propagated through ChildNo up until the toplevel. where it acts
  // like a regular no
  protected case class Never(name: String) extends CodePathCheckResult {
    override val success: Boolean = false
    override val never: Boolean = true
    override val cause: Option[CodePathCheckResult] = None
  }
  
  protected case class ChildNo(name: String, child: CodePathCheckResult) extends CodePathCheckResult {
    override val success: Boolean = false
    override val never: Boolean = child.never
    override val cause: Option[CodePathCheckResult] = Some(child)
  }
}

sealed trait CodePathCheckResult {
  
  val success: Boolean
  val never: Boolean
  val cause: Option[CodePathCheckResult]
}