package extintcode.util

class HeaderModule(override val name: String,
                   override val version: Int,
                   fieldSet: List[(FieldEntry, Long)],
                   functionSet: List[(FunctionEntry, Long)]
                  ) extends Module {
  val fields: Map[String, (FieldEntry, Long)] = fieldSet.map(x => (x._1.name, x)).toMap
  val functions: Map[(String, Int), (FunctionEntry, Long)] = functionSet.map(x => ((x._1.name, x._1.args), x)).toMap
  val names: Set[String] = fieldSet.map(_._1.name).appendedAll(functionSet.map(_._1.name)).toSet
  
  def getField(name: String): (FieldEntry, Long) = fields.getOrElse(name, throw new NoSuchElementException("Field not found in module " + name + ": " + name))
  def getFunc(name: String, params: Int): (FunctionEntry, Long) = functions.getOrElse((name, params), throw new NoSuchElementException("Field not found in module " + name + ": " + name))
  
  def checkName(name: String): Unit = if (!names.contains(name)) throw new NoSuchElementException("Module " + name + " has no exported memeber: " + name)

}
