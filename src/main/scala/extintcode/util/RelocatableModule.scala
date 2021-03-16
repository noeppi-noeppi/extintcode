package extintcode.util

class RelocatableModule(override val name: String,
                        override val version: Int,
                        val dependencies: Map[String, Int],
                        val maxFunc: Int,
                        val code: Seq[(Long, String)]
                       ) extends Module {
  
}
