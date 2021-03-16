package extintcode.asm

import extintcode.asm.IntCodeAssembler._

sealed trait AssemblerStatement extends AssemblyText {
  val size: Int
  def code(data: LabelData): Seq[(Long, String)]
}

case class StmtAdd(in1: ValType, in2: ValType, out: ValType) extends AssemblerStatement {
  override val size: Int = 4
  override def code(data: LabelData): Seq[(Long, String)] = Seq(
    opcode(1, in1, in2, out), in1(data), in2(data), out(data)
  )
  override def textStr(): String = s"        add     ${out.string()}, ${in1.string()}, ${in2.string()}"
}

case class StmtMul(in1: ValType, in2: ValType, out: ValType) extends AssemblerStatement {
  override val size: Int = 4
  override def code(data: LabelData): Seq[(Long, String)] = Seq(
    opcode(2, in1, in2, out), in1(data), in2(data), out(data)
  )
  override def textStr(): String = s"        mul     ${out.string()}, ${in1.string()}, ${in2.string()}"
}

case class StmtInp(out: ValType) extends AssemblerStatement {
  override val size: Int = 2
  override def code(data: LabelData): Seq[(Long, String)] = Seq(
    opcode(3, out), out(data)
  )
  override def textStr(): String = s"        inp     ${out.string()}"
}

case class StmtOutp(in: ValType) extends AssemblerStatement {
  override val size: Int = 2
  override def code(data: LabelData): Seq[(Long, String)] = Seq(
    opcode(4, in), in(data)
  )
  override def textStr(): String = s"        outp    ${in.string()}"
}

case class StmtJnz(in: ValType, target: ValType) extends AssemblerStatement {
  override val size: Int = 3
  override def code(data: LabelData): Seq[(Long, String)] = Seq(
    opcode(5, in, target), in(data), target(data)
  )
  override def textStr(): String = s"        jnz     ${target.string()}, ${in.string()}"
}

case class StmtJz(in: ValType, target: ValType) extends AssemblerStatement {
  override val size: Int = 3
  override def code(data: LabelData): Seq[(Long, String)] = Seq(
    opcode(6, in, target), in(data), target(data)
  )
  override def textStr(): String = s"        jz      ${target.string()}, ${in.string()}"
}

case class StmtLt(in1: ValType, in2: ValType, out: ValType) extends AssemblerStatement {
  override val size: Int = 4
  override def code(data: LabelData): Seq[(Long, String)] = Seq(
    opcode(7, in1, in2, out), in1(data), in2(data), out(data)
  )
  override def textStr(): String = s"        lt      ${out.string()}, ${in1.string()}, ${in2.string()}"
}

case class StmtEq(in1: ValType, in2: ValType, out: ValType) extends AssemblerStatement {
  override val size: Int = 4
  override def code(data: LabelData): Seq[(Long, String)] = Seq(
    opcode(8, in1, in2, out), in1(data), in2(data), out(data)
  )
  override def textStr(): String = s"        eq      ${out.string()}, ${in1.string()}, ${in2.string()}"
}

case class StmtRel(in: ValType) extends AssemblerStatement {
  override val size: Int = 2
  override def code(data: LabelData): Seq[(Long, String)] = Seq(
    opcode(9, in), in(data)
  )
  override def textStr(): String = s"        rel     ${in.string()}"
}

object StmtRet extends AssemblerStatement {
  override val size: Int = 1
  override def code(data: LabelData): Seq[(Long, String)] = Seq(
    (99, null)
  )
  override def textStr(): String = s"        ret"
}

case class StmtMov(in: ValType, out: ValType) extends AssemblerStatement {
  override val size: Int = 4
  override def code(data: LabelData): Seq[(Long, String)] = Seq(
    opcode(1, in, Direct(0, null), out), in(data), Direct(0, null)(data), out(data)
  )
  override def textStr(): String = s"        mov     ${out.string()}, ${in.string()}"
}

case class StmtJmp(target: ValType) extends AssemblerStatement {
  override val size: Int = 3
  override def code(data: LabelData): Seq[(Long, String)] = Seq(
    opcode(6, Direct(0, null), target), Direct(0, null)(data), target(data)
  )
  override def textStr(): String = s"        jmp     ${target.string()}"
}

case class StmtPush(in: ValType) extends AssemblerStatement {
  override val size: Int = 6
  override def code(data: LabelData): Seq[(Long, String)] = Seq(
    // First shift CALLSTACK as after a rel statement the input value may differ
    opcode(1, CALLSTACK, in, CALLSTACK), CALLSTACK(data), in(data), CALLSTACK(data),
    opcode(9, in), in(data)
  )
  override def textStr(): String = s"        push    ${in.string()}"
}

case class StmtPop(in: ValType) extends AssemblerStatement {
  override val size: Int = in match {
    case Direct(_, null) => 6
    case _ => 10
  }
  override def code(data: LabelData): Seq[(Long, String)] = {
    val (neg, prefix) = in match {
      case Direct(num, null) => (Direct(-num, null), Seq())
      case _ => (X1, Seq(
        opcode(2, in, Direct(-1, null), X1), in(data), Direct(-1, null)(data), X1(data)
      ))
    }
    prefix.appendedAll(Seq(
      // First shift CALLSTACK as after a rel statement the input value may differ
      opcode(1, CALLSTACK, neg, CALLSTACK), CALLSTACK(data), neg(data), CALLSTACK(data),
      opcode(9, neg), neg(data)
    ))
  }
  override def textStr(): String = s"        pop     ${in.string()}"
}

case class StmtDyn(amount: ValType, target: ValType) extends AssemblerStatement {
  override val size: Int = 8
  override def code(data: LabelData): Seq[(Long, String)] = {
    Seq(
      opcode(1, NEXTDYN, Direct(0, null), target), NEXTDYN(data), Direct(0, null)(data), target(data),
      opcode(1, NEXTDYN, amount, NEXTDYN), NEXTDYN(data), amount(data), NEXTDYN(data)
    )
  }
  override def textStr(): String = s"        dyn     ${target.string()}, ${amount.string()}"
}

case class StmtLoad(pointer: ValType, out: ValType) extends AssemblerStatement {

  // -1 * CALLSTACK -> X1
  // pointer -> X2
  // rel X1
  // rel X2
  // r0 -> X1
  // -1 * X2 -> X2
  // rel X2
  // rel CALLSTACK
  // X1 -> out
  
  if (pointer.mode == 1) println("IntCode Assembler: Warning: Using load with direct mode pointer")
  
  override val size: Int = 28
  override def code(data: LabelData): Seq[(Long, String)] = {
    Seq(
      opcode(2, CALLSTACK, Direct(-1, null), X1), CALLSTACK(data), Direct(-1, null)(data), X1(data),
      opcode(1, pointer, Direct(0, null), X2), pointer(data), Direct(0, null)(data), X2(data),
      opcode(9, X1), X1(data),
      opcode(9, X2), X2(data),
      ((Math.pow(10, 4).toLong * X1.mode) + 1201, null), (0, null), (0, null), X1(data),
      opcode(2, X2, Direct(-1, null), X2), X2(data), Direct(-1, null)(data), X2(data),
      opcode(9, X2), X2(data),
      opcode(9, CALLSTACK), CALLSTACK(data),
      opcode(1, X1, Direct(0, null), out), X1(data), Direct(0, null)(data), out(data)
    )
  }
  override def textStr(): String = s"        load    ${out.string()}, ${pointer.string()}"
}

case class StmtStore(in: ValType, pointer: ValType) extends AssemblerStatement {

  // As we only have two values for free use we temporarily modify CALLSTACK
  // in -> X1
  // pointer -> X2
  // -1 * CALLSTACK -> CALLSTACK
  // rel CALLSTACK
  // rel X2
  // X1 -> r0
  // -1 * X2 -> X2
  // -1 * CALLSTACK -> CALLSTACK
  // rel X2
  // rel CALLSTACK
  
  if (pointer.mode == 1) println("IntCode Assembler: Warning: Using store with direct mode pointer")

  override val size: Int = 32
  override def code(data: LabelData): Seq[(Long, String)] = {
    Seq(
      opcode(1, in, Direct(0, null), X1), in(data), Direct(0, null)(data), X1(data),
      opcode(1, pointer, Direct(0, null), X2), pointer(data), Direct(0, null)(data), X2(data),
      opcode(2, CALLSTACK, Direct(-1, null), CALLSTACK), CALLSTACK(data), Direct(-1, null)(data), CALLSTACK(data),
      opcode(9, CALLSTACK), CALLSTACK(data),
      opcode(9, X2), X2(data),
      ((Math.pow(10, 2).toLong * X1.mode) + 21001, null), X1(data), (0, null), (0, null),
      opcode(2, X2, Direct(-1, null), X2), X2(data), Direct(-1, null)(data), X2(data),
      opcode(2, CALLSTACK, Direct(-1, null), CALLSTACK), CALLSTACK(data), Direct(-1, null)(data), CALLSTACK(data),
      opcode(9, X2), X2(data),
      opcode(9, CALLSTACK), CALLSTACK(data)
    )
  }
  override def textStr(): String = s"        store   ${pointer.string()}, ${in.string()}"
}

case class StmtCall(target: ValType) extends AssemblerStatement {
  override val size: Int = 7
  override def code(data: LabelData): Seq[(Long, String)] = {
    Seq(
      opcode(1, Direct(data.currentInst + size, ""), Direct(0, null), BACKJUMP),  Direct(data.currentInst + size, "")(data), Direct(0, null)(data), BACKJUMP(data),
      opcode(6, Direct(0, null), target), Direct(0, null)(data), target(data)
    )
  }
  override def textStr(): String = s"        call    ${target.string()}"
}

case class StmtRaw(ints: Seq[(Long, String)]) extends AssemblerStatement {
  override val size: Int = ints.size
  override def code(data: LabelData): Seq[(Long, String)] = ints
  override def textStr(): String = s"        raw     ${ints.map(x => Direct(x._1, x._2)).map(_.string()).mkString(", ")}"
}