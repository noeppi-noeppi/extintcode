package extintcode.compile.literal

class LiteralBool(value: Boolean) extends LiteralInt(if (value) 1 else 0) {

}
