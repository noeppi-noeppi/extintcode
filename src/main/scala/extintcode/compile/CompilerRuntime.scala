package extintcode.compile

import extintcode.asm._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class CompilerRuntime {
  
  private val labels = mutable.Map[String, Int]()
  private val dataEntries = mutable.Map[String, Int]()
  
  private var variablesStackSize = -1
  private val variableStack = mutable.Stack[(mutable.Map[String, Variable], Boolean)]()
  private val variableData = ListBuffer[AssemblyData]()
  private val allVariables = mutable.Set[Variable]()
  private var stackResultType: Boolean = false
  private val controlStack = mutable.Stack[ControlJumps]()
  
  private var expressionStackSize = -1
  
  def newLabel(label: String): String = {
    if (label.last.isDigit) {
      throw new IllegalArgumentException("Internal compiler error: Label base name may not end with a number")
    }
    if (!labels.contains(label)) {
      labels.put(label, 0)
      label
    } else if (labels(label) < 0) {
      throw new IllegalStateException("Internal compiler error: Special label can not be overwritten with regular label.")
    } else {
      val suffix = "_" + labels(label)
      labels.put(label, labels(label) + 1)
      label + suffix
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
    if (data.last.isDigit) {
      throw new IllegalArgumentException("Internal compiler error: Data Entry base name may not end with a number")
    }
    if (!dataEntries.contains(data)) {
      dataEntries.put(data, 0)
      data
    } else {
      val suffix = "_" + dataEntries(data)
      dataEntries.put(data, dataEntries(data) + 1)
      data + suffix
    }
  }
  
  def createVariable(name: String, pointer: Boolean): Variable = {
    if (expressionStackSize >= 0) {
      throw new IllegalStateException("Internal compiler error: Can't create variable inside expression.")
    }
    val scope = currentScope()
    if (scope.contains(name)) throw new IllegalStateException("Variable " + name + " is already defined in the scope.")
    // Take all layers of teh stack including the first one that allows for redefinitions
    for ((parent, _) <- variableStack.take(variableStack.indexWhere(_._2) + 1)) {
      if (parent.contains(name)) throw new IllegalStateException("Variable " + name + " con not be redefined at this place.")
    }
    if (variablesStackSize >= 0) {
      val v = new Variable(name, MemoryStack(variablesStackSize, null), pointer, false)
      variablesStackSize += 1
      scope.put(name, v)
      allVariables.add(v)
      if (name.startsWith("~")) v.noConst()
      v
    } else {
      val dataEntry = newDataEntry("var_" + name)
      val v = new Variable(name, MemoryData(dataEntry), pointer, false)
      variableData.addOne(DataBySize(dataEntry, 1))
      scope.put(name, v)
      allVariables.add(v)
      if (name.startsWith("~")) v.noConst()
      v
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
    scope.put(v.name, v)
  }
  
  def createConstant(name: String, value: Long): Variable = {
    val scope = currentScope()
    val v = new Variable(name, Direct(value, null), false, true)
    scope.put(name, v)
      allVariables.add(v)
    v
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
  
  def pushScope(): Unit = this.pushScope(false)
  
  private def pushScope(allowsRedefinition: Boolean): Unit = {
    variableStack.push((mutable.Map(), allowsRedefinition))
  }
  
  def popScope(): Unit = {
    if (variablesStackSize >= 0) {
      variablesStackSize -= variableStack.head._1.count(!_._2.const)
    }
    variableStack.pop()
  }
  
  def startStackSection(result: Boolean): Unit = {
    if (variablesStackSize >= 0) {
      throw new IllegalStateException("Internal compiler error: Nested stack sections are not allowed.")
    }
    variablesStackSize = 0
    stackResultType = result
    pushScope(true)
  }
  
  def endStackSection(): Unit = {
    if (variablesStackSize < 0) {
      throw new IllegalStateException("Internal compiler error: Can't end stack section without start.")
    }
    popScope()
    if (variablesStackSize != 0) {
      throw new IllegalStateException("Variable stack handling error.")
    }
    variablesStackSize = -1
    stackResultType = false
  }
  
  def call(jmp: ValType): List[AssemblyText] = {
    val totalStackSize = Math.max(0, variablesStackSize) + Math.max(0, expressionStackSize)
    val code = ListBuffer[AssemblyText]()
    if (totalStackSize > 0) {
      code.addOne(StmtPush(Direct(totalStackSize, null)))
    }
    code.addOne(StmtCall(jmp))
    if (totalStackSize > 0) {
      code.addOne(StmtPop(Direct(totalStackSize, null)))
    }
    code.toList
  }
  
  def startExpressionSection(): Unit = {
    if (expressionStackSize >= 0) {
      throw new IllegalStateException("Internal compiler error: Nested expression sections are not allowed.")
    }
    expressionStackSize = 0
  }
  
  def endExpressionSection(): Unit = {
    if (expressionStackSize < 0) {
      throw new IllegalStateException("Internal compiler error: Can't end expression section without start.")
    }
    expressionStackSize = -1
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
  
  def getCollectedDataEntries: List[AssemblyData] = variableData.toList
  
  def getPossiblyConstantVars: Set[Variable] = allVariables.filter(!_.const).filter(!_.name.startsWith("~")).filter(_.canBeConst).toSet
  
  def stackSectionResultType: Option[Boolean] = if (variablesStackSize >= 0) Some(stackResultType) else None
  
  def getControl: ControlJumps = {
    if (controlStack.isEmpty) {
      throw new IllegalStateException("Can't use break / continue otside a loop.")
    }
    controlStack.head
  }
  
  def pushControl(control: ControlJumps): Unit = {
    controlStack.push(control)
  }
  
  def popControl(): Unit = {
    controlStack.pop()
  }
}
