package extintcode.compile

abstract class Operator[T](val name: String, val priority: Priority) {
  
  def apply(in1: T, in2: T): T
}
