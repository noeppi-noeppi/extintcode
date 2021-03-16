package extintcode.compile

import extintcode.asm.ValType

// break: leaves the loop
// continue: Jumps before the condition check: Next iteration but with condition
// next: Jumps below the condition heck: Next iteration but without the condition being checked again.
case class ControlJumps(break: ValType, continue: ValType, next: ValType) {

}
