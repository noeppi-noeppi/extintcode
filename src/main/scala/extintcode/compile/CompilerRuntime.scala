package extintcode.compile

import extintcode.asm._
import extintcode.compile.frame.{EndFrame, ExpressionFrame, Frame, ScopeFrame, StackFrame, VariableFrame}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class CompilerRuntime {
  
  private val labels = mutable.Map[String, Int]()
  private val dataEntries = mutable.Map[String, Int]()
  
  private var variablesStackSize = -1
  private val variableStack = mutable.Stack[(mutable.Map[String, Variable], Boolean)]()
  private val literalData = ListBuffer[AssemblyData]()
  private val variableData = ListBuffer[AssemblyData]()
  private val allVariables = ListBuffer[Variable]()
  private var stackResultType: Boolean = false
  private val controlStack = mutable.Stack[ControlJumps]()
  
  private var expressionStackSize = -1
  
  private val stringLiterals = mutable.Map[String, String]()
  
  def newLabel(label: String): String = {
    val l = if (label.last.isDigit) {
      label + "_"
    } else {
      label
    }
    if (!labels.contains(l)) {
      labels.put(l, 0)
      l
    } else if (labels(l) < 0) {
      throw new IllegalStateException("Internal compiler error: Special label can not be overwritten with regular label.")
    } else {
      val suffix = "_" + labels(l)
      labels.put(l, labels(l) + 1)
      l + suffix
    }
  }

  def specialLabel(label: String): Unit = {
    if (label.last.isDigit) {
      throw new IllegalArgumentException("Internal compiler error: Special label name may not end with a number")
    }
    if (!labels.contains(label)) {
      labels.put(label, -1)
    } else {
      throw new IllegalStateException("Internal compiler error: Special label name not unique")
    }
  }
  
  def newDataEntry(data: String): String = {
    val d = if (data.last.isDigit) {
      data + "_"
    } else {
      data
    }
    if (!dataEntries.contains(d)) {
      dataEntries.put(d, 0)
      d
    } else {
      val suffix = "_" + dataEntries(d)
      dataEntries.put(d, dataEntries(d) + 1)
      d + suffix
    }
  }
  
  def createVariable(name: String, pointer: Boolean): (Variable, List[Frame]) = {
    if (expressionStackSize >= 0) {
      throw new IllegalStateException("Internal compiler error: Can't create variable inside expression.")
    }
    val scope = currentScope()
    checkName(name)
    if (variablesStackSize >= 0) {
      val v = new Variable(name, MemoryStack(variablesStackSize, null), pointer, false)
      variablesStackSize += 1
      scope.put(name, v)
      allVariables.addOne(v)
      if (name.startsWith("~")) v.noConst()
      (v, List(VariableFrame(name, v.location)))
    } else {
      val dataEntry = newDataEntry("var_" + name)
      val v = new Variable(name, MemoryData(dataEntry), pointer, false)
      variableData.addOne(DataBySize(dataEntry, 1))
      scope.put(name, v)
      allVariables.addOne(v)
      if (name.startsWith("~")) v.noConst()
      (v, List(VariableFrame(name, v.location)))
    }
  }
  
  def addGlobalVariable(v: Variable): Unit = {
    if (expressionStackSize >= 0) {
      throw new IllegalStateException("Internal compiler error: Can't create global variable inside expression.")
    }
    if (variablesStackSize >= 0 || variableStack.size >= 2) {
      throw new IllegalStateException("Global or exported variables can only be created toplevel: " + v.name)
    }
    val scope = currentScope()
    checkName(v.name)
    scope.put(v.name, v)
  }
  
  def createConstant(name: String, value: Long): Variable = {
    val scope = currentScope()
    checkName(name)
    val v = new Variable(name, Direct(value, null), false, true)
    scope.put(name, v)
      allVariables.addOne(v)
    v
  }
  
  private def checkName(name: String): Unit = {
    val scope = currentScope()
    if (scope.contains(name)) throw new IllegalStateException("Variable " + name + " is already defined in the scope.")
    // Take all layers of the stack including the first one that allows for redefinitions
    for ((parent, _) <- variableStack.take(variableStack.indexWhere(_._2) + 1)) {
      if (parent.contains(name)) throw new IllegalStateException("Variable " + name + " con not be redefined at this place.")
    }
  }
  
  def getVariable(name: String): Variable = {
    for ((scope, _) <- variableStack) {
      if (scope contains name)
        return scope(name)
    }
    if (name.startsWith("~")) {
      throw new IllegalStateException("Internal compiler error: Internal variable " + name.substring(1) + " not found.")
    } else {
      throw new NoSuchElementException("Variable " + name + " not defined.")
    }
  }
  
  private def currentScope(): mutable.Map[String, Variable] = {
    if (variableStack.isEmpty) {
      throw new IllegalStateException("No variable scope has been initialised yet.")
    }
    variableStack.head._1
  }
  
  def pushInitialScope(): Unit = {
    if (variableStack.nonEmpty) {
      throw new IllegalStateException("Internal compiler error: Can't push initial scope on non-empty stack.")
    }
    pushScope(true)
  }
  
  def pushScope(): List[Frame] = {
    val lastVariables = getLastVariables
    pushScope(false)
    List(ScopeFrame(lastVariables))
  }
  
  private def getLastVariables: Int = {
    if (variablesStackSize >= 0) {
      variableStack.headOption.map(_._1.size).getOrElse(0)
    } else {
      0
    }
  }
  
  private def pushScope(allowsRedefinition: Boolean): Unit = {
    variableStack.push((mutable.Map(), allowsRedefinition))
  }
  
  def popScope(): List[Frame] = {
    if (variablesStackSize >= 0) {
      variablesStackSize -= variableStack.head._1.count(!_._2.const)
    }
    variableStack.pop()
    List(EndFrame)
  }
  
  def startStackSection(result: Boolean): List[Frame] = {
    if (variablesStackSize >= 0) {
      throw new IllegalStateException("Internal compiler error: Nested stack sections are not allowed.")
    }
    val lastVariables = getLastVariables
    variablesStackSize = 0
    stackResultType = result
    pushScope(true)
    List(StackFrame(lastVariables))
  }
  
  def endStackSection(): List[Frame] = {
    if (variablesStackSize < 0) {
      throw new IllegalStateException("Internal compiler error: Can't end stack section without start.")
    }
    popScope()
    if (variablesStackSize != 0) {
      throw new IllegalStateException("Variable stack handling error.")
    }
    variablesStackSize = -1
    stackResultType = false
    List(EndFrame)
  }
  
  def stackSection(): Boolean = variablesStackSize >= 0
  
  def startExpressionSection(): List[Frame] = {
    if (expressionStackSize >= 0) {
      throw new IllegalStateException("Internal compiler error: Nested expression sections are not allowed.")
    }
    val lastVariables = getLastVariables
    expressionStackSize = 0
    List(ExpressionFrame(lastVariables))
  }
  
  def endExpressionSection(): List[Frame] = {
    if (expressionStackSize < 0) {
      throw new IllegalStateException("Internal compiler error: Can't end expression section without start.")
    }
    expressionStackSize = -1
    List(EndFrame)
  }
  
  def createExpressionResult(): ValType = {
    if (expressionStackSize < 0) {
      throw new IllegalStateException("Internal compiler error: Can't allocate expression result value outside expression section.")
    }
    val x = MemoryStack(Math.max(0, variablesStackSize) + expressionStackSize, null)
    expressionStackSize += 1
    x
  }
  
  def stackSizeForPush(pure: Boolean, statement: Boolean): Option[Int] = {
    if (pure) {
      None
    } else if (statement != (expressionStackSize < 0)) {
      throw new IllegalStateException("Internal Compiler Error: Function stack push as statement inside expression or function expression while not in expressin mode.")
    } else {
      Some((0 max variablesStackSize) + (0 max expressionStackSize)).filter(_ != 0)
    }
  }
  
  def checkType(action: String, expected: Boolean, actual: Boolean): Unit = {
    if (expected && !actual) {
      throw new IllegalStateException("Type mismatch in " + action + ": pointer required.")
    } else if (!expected && actual) {
      throw new IllegalStateException("Type mismatch in " + action + ": direct value required (Probably a missing pointer dereferenciation).")
    }
  }
  
  def negateCompileTime(v: ValType): Option[ValType] = v match {
    case Direct(n, null) => Some(Direct(-n, null))
    case _ => None
  }
  
  def getCollectedDataEntries: List[AssemblyData] = List.newBuilder.addAll(literalData).addAll(variableData).result()
  
  def getPossiblyConstantVars: List[Variable] = allVariables.filter(!_.const).filter(!_.name.startsWith("~")).filter(_.canBeConst).toList
  
  def stackSectionResultType: Option[Boolean] = if (variablesStackSize >= 0) Some(stackResultType) else None
  
  def getControl: ControlJumps = {
    if (controlStack.isEmpty) {
      throw new IllegalStateException("Can't use break / continue outside a loop.")
    }
    controlStack.head
  }
  
  def pushControl(control: ControlJumps): Unit = {
    controlStack.push(control)
  }
  
  def popControl(): Unit = {
    controlStack.pop()
  }
  
  def createStringLiteral(str: String): String = {
    if (!stringLiterals.contains(str)) {
      val name = newDataEntry("literal")
      stringLiterals.put(str, name)
      literalData.addOne(DataByValue(name, DataString(str)))
      name
    } else {
      stringLiterals(str)
    }
  }
}
