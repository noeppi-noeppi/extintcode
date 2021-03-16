package extintcode.util

class ModuleResolutionException(msg: String, val module: String,  val parent: Option[String]) extends RuntimeException(msg) {
  
}
