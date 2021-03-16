package extintcode.asm

sealed trait ValType {
  val mode: Int
  def apply(data: LabelData): (Long, String)
  def string(): String
}

case class Direct(value: Long, relocate: String) extends ValType {
  override val mode: Int = 1
  override def apply(data: LabelData): (Long, String) = (value, relocate)
  override def string(): String = (if (relocate == null) "" else relocate + "%") + value.toString
}

case class Memory(value: Long, relocate: String) extends ValType {
  override val mode: Int = 0
  override def apply(data: LabelData): (Long, String) = (value, relocate)
  override def string(): String = "[" + (if (relocate == null) "" else relocate + "%") + value.toString + "]"
}

case class MemoryStack(value: Long, relocate: String) extends ValType {
  override val mode: Int = 2
  override def apply(data: LabelData): (Long, String) = (value, relocate)
  override def string(): String = "[*" + (if (relocate == null) "" else relocate + "%") + value.toString + "]"
}

case class DirectLabel(value: String) extends ValType {
  override val mode: Int = 1
  override def apply(data: LabelData): (Long, String) = (data.labelAddress(value), "")
  override def string(): String = "&" + value
}

case class MemoryLabel(value: String) extends ValType {
  override val mode: Int = 0
  override def apply(data: LabelData): (Long, String) = (data.labelAddress(value), "")
  override def string(): String = "[&" + value + "]"
}

case class DirectData(value: String) extends ValType {
  override val mode: Int = 1
  override def apply(data: LabelData): (Long, String) = (data.dataAddress(value), "")
  override def string(): String = "!" + value
}

case class MemoryData(value: String) extends ValType {
  override val mode: Int = 0
  override def apply(data: LabelData): (Long, String) = (data.dataAddress(value), "")
  override def string(): String = "[!" + value + "]"
}

case class SpecialValue(value: String) extends ValType {
  override val mode: Int = 0
  override def apply(data: LabelData): (Long, String) = IntCodeAssembler.parseSpecialValue(value, if (data.hasDependencies) Int.MaxValue else data.maxFunc)(data)
  override def string(): String = value
}

case class SpecialValueAddress(value: String) extends ValType {
  override val mode: Int = 1
  override def apply(data: LabelData): (Long, String) = IntCodeAssembler.parseSpecialValue(value, if (data.hasDependencies) Int.MaxValue else data.maxFunc)(data)
  override def string(): String = "\\" + value
}