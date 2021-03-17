package extintcode

import extintcode.asm.IntCodeAsm
import extintcode.compile.IntCodeCompiler
import extintcode.link.IntCodeLinker
import extintcode.vm.IntCodeVM

object Main extends App {

  if (args.length == 0) {
    println("ExtIntCode2")
    println()
    println("Choose a sub-command:")
    println("  vm        Runs the IntCode Interpreter / VM")
    println("  link      Links a Relocatable IntCode file to produce executable IntCode")
    println("  asm       Assembles IntCode Assembly to produce Relocatable IntCode")
    println("  compile   Compiles an extintcode source file to IntCode Assembler")
  } else {
    args(0).toLowerCase match {
      case "vm" => IntCodeVM.run(args.tail.toIndexedSeq: _*)
      case "link" => IntCodeLinker.run(args.tail.toIndexedSeq: _*)
      case "asm" => IntCodeAsm.run(args.tail.toIndexedSeq: _*)
      case "compile" => IntCodeCompiler.run(args.tail.toIndexedSeq: _*)
      case x => println("Unknown sub-command: '" + x + "'")
    }
  }
}
