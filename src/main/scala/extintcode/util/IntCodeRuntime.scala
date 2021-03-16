package extintcode.util

object IntCodeRuntime {

  val NEXTDYN: Int = 16
  val CALLSTACK: Int = 17
  val BACKJUMP: Int = 18
  val RETURN: Int = 19
  val X1: Int = 22
  val X2: Int = 23
  val GLOBAL1: Int = 24
  val GLOBAL2: Int = 25
  val GLOBAL3: Int = 26
  val GLOBAL4: Int = 27
  val GLOBAL5: Int = 28
  val GLOBAL6: Int = 29
  val GLOBAL7: Int = 30
  val GLOBAL8: Int = 31
  val PARAM_BASE: Int = 32
  
  object Names {
    val NEXTDYN: String = "NEXTDYN"
    val CALLSTACK: String = "CALLSTACK"
    val BACKJUMP: String = "BACKJUMP"
    val RETURN: String = "RETURN"
    val X1: String = "X1"
    val X2: String = "X2"
    val GLOBAL1: String = "GLOBAL1"
    val GLOBAL2: String = "GLOBAL2"
    val GLOBAL3: String = "GLOBAL3"
    val GLOBAL4: String = "GLOBAL4"
    val GLOBAL5: String = "GLOBAL5"
    val GLOBAL6: String = "GLOBAL6"
    val GLOBAL7: String = "GLOBAL7"
    val GLOBAL8: String = "GLOBAL8"
  }
  
  object Modules {
    val BUILTIN = "builtin"
    val LOCAL = "local"
  }
}
