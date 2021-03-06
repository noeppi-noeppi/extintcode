package extintcode.compile

import extintcode.asm.ValType

// break: leaves the loop
// continue: Jumps before the condition check: Next iteration but with condition
// next: Jumps below the condition check: Next iteration but without the condition being checked again.
case class ControlJumps(break: Either[ValType, String], continue: Either[ValType, String], next: Either[ValType, String]) {

}
