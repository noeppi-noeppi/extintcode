package extintcode.asm

case class LabelData(currentInst: Int, maxFunc: Int, hasDependencies: Boolean, private val labels: Map[String, Int], private val dataEntries: Map[String, Int]) {

  def labelAddress(label: String): Int = labels(label)
  def dataAddress(data: String): Int = dataEntries(data)
}
