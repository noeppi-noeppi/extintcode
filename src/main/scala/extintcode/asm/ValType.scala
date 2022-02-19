package extintcode.asm

sealed trait ValType {
  val mode: Int
  def apply(data: LabelData): (Long, String)
  def string(): String
  def directValue(): Option[Long] = None
  def immediatePointer(): Option[ValType] = None
}

case class Direct(value: Long, relocate: String) extends ValType {
  override val mode: Int = 1
  override def apply(data: LabelData): (Long, String) = (value, relocate)
  override def string(): String = (if (relocate == null) "" else relocate + "%") + value.toString
  override def directValue(): Option[Long] = Some(value)
  override def immediatePointer(): Option[ValType] = Some(Memory(value, relocate))
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
  override def immediatePointer(): Option[ValType] = Some(MemoryLabel(value))
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
  override def immediatePointer(): Option[ValType] = Some(MemoryData(value))
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
  override def apply(data: LabelData): (Long, String) = IntCodeAssembler.parseSpecialAddress(value, if (data.hasDependencies) Int.MaxValue else data.maxFunc)(data)
  override def string(): String = "\\" + value
  override def directValue(): Option[Long] = IntCodeAssembler.parseSpecialAddress(value, Int.MaxValue).directValue()
  override def immediatePointer(): Option[ValType] = Some(SpecialValue(value))
}