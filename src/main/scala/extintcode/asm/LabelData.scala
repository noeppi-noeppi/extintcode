package extintcode.asm

case class LabelData(currentInst: Int, maxFunc: Int, hasDependencies: Boolean, private val labels: Map[String, Int], private val dataEntries: Map[String, Int]) {

  def labelAddress(label: String): Int = labels.getOrElse(label, throw new NoSuchElementException("Assembler label '" + label + "' not found."))
  def dataAddress(data: String): Int = dataEntries.getOrElse(data, throw new NoSuchElementException("Assembler data definition '" + data + "' not found."))
}
