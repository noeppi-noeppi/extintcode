package extintcode.vm

import extintcode.util.{IntCodeFlags, IntCodeIO}

import java.io.{BufferedReader, DataInput, DataInputStream}
import java.nio.file.{Files, Path}
import scala.collection.mutable.ListBuffer

class IntCodeInterpreter(val memory: Memory, val flags: Long) {

  val ascii: Boolean = (flags & IntCodeFlags.ASCII) != 0
  val debug: Boolean = (flags & IntCodeFlags.DEBUG) != 0
  val exd: Boolean = (flags & IntCodeFlags.EXD) != 0
  
  private var instruction: Int = 0
  private var relative: Int = 0
  private val section: Int = memory.buffer.length

  def run(): Unit = {
    try {
      while (true) {
        val oldPointer = instruction
        val params = try {
          debug("INSTRUCTION ", instruction)
          makeInstruction()
        } catch {
          case e: PartialSegmentationFault => segv(e.accessTry, e.getMessage)
        }
        if (instruction == oldPointer) {
          // No JUMP was called. Next instruction please.
          debug("JUMP NEXT")
          instruction += 1 + params
        }
      }
      throw new Error
    } catch {
      case _: ExitException =>
    }
  }

  private def makeInstruction(): Int = {
    val opcode = memory(instruction)
    if (opcode < 0) segv(instruction, "INSTRUCTION IS NEGATIVE")
    debug("OPCODE ", opcode)
    opcode % 100 match {
      case 0 => segv(instruction, "NULL INSTRUCTION")
      case 1 => write(opcode, 3, read(opcode, 1) + read(opcode, 2)); 3
      case 2 => write(opcode, 3, read(opcode, 1) * read(opcode, 2)); 3
      case 3 if !ascii => write(opcode, 1, readInt()); 1
      case 3 if ascii => write(opcode, 1, readChar()); 1
      case 4 if !ascii => println(read(opcode, 1)); 1
      case 4 if ascii => val cp = check32(read(opcode, 1), "INVALID CODE POINT OUTPUT"); if (Character.isValidCodePoint(cp)) { print(new String(Character.toChars(cp))) } else { segv(instruction + 1, "INVALID CODE POINT OUTPUT") }; 1
      case 5 => if (read(opcode, 1) != 0) instruction = check32(read(opcode, 2), "JUMP OUT OF MEMORY"); 2
      case 6 => if (read(opcode, 1) == 0) instruction = check32(read(opcode, 2), "JUMP OUT OF MEMORY"); 2
      case 7 => write(opcode, 3, if (read(opcode, 1) < read(opcode, 2)) { 1 } else { 0 }); 3
      case 8 => write(opcode, 3, if (read(opcode, 1) == read(opcode, 2)) { 1 } else { 0 }); 3
      case 9 => relative += check32(read(opcode, 1), "RELATIVE MEMORY OVERFLOW"); debug("RELATIVE ", relative); 1
      case 99 => if (exd) { segv(instruction, "EXIT 0") } else { throw new ExitException }
      case _ => segv(instruction, "UNKNOWN OPCODE")
    }
  }

  private def read(opcode: Long, paramNum: Int): Long = {
    debug("READ ", paramNum)
    val param = memory(instruction + paramNum)
    (opcode / Math.pow(10, paramNum + 1).toInt) % 10 match {
      case 0 =>
        debug("MEMORY ", param)
        val value = memory(param)
        debug("VALUE ", value)
        value
      case 1 =>
        debug("VALUE ", param)
        param
      case 2 =>
        debug("MEMORY ", relative + param)
        val value = memory(relative + param)
        debug("VALUE ", value)
        value
      case _ => segv(instruction + paramNum, "UNKNOWN READ PARAMETER MODE")
    }
  }

  private def write(opcode: Long, paramNum: Int, value: Long): Unit = {
    debug("WRITE ", paramNum)
    debug("VALUE ", value)
    val param = memory(instruction + paramNum)
    (opcode / Math.pow(10, paramNum + 1).toInt) % 10 match {
      case 0 =>
        debug("MEMORY ", param)
        memory(param) = value
      case 1 =>
        segv(instruction + paramNum, "DIRECT MODE USED FOR WRITING")
      case 2 =>
        debug("MEMORY ", relative + param)
        memory(relative + param) = value
      case _ => segv(instruction + paramNum, "UNKNOWN WRITE PARAMETER MODE")
    }
  }

  private def readInt(): Long = {
    val lb = ListBuffer[Int]()
    var in = System.in.read()
    while (in != '\n') {
      lb += in
      in = System.in.read()
    }
    try {
      java.lang.Long.parseLong(new String(lb.flatMap(i => Character.toChars(i)).toArray))
    } catch {
      case _: NumberFormatException => segv(-1, "INVALID INPUT")
    }
  }

  private def readChar(): Int = {
    val bytes = ListBuffer[Byte]()
    bytes += System.in.read().toByte
    val count =
      if (bytes.head >= 0) 1
      else if ((bytes.head >= 0xc0.toByte) && (bytes.head <= 0xdf.toByte)) 2
      else if ((bytes.head >= 0xe0.toByte) && (bytes.head <= 0xef.toByte)) 3
      else if ((bytes.head >= 0xf0.toByte) && (bytes.head <= 0xf7.toByte)) 4
      else segv(instruction, "INVALID CODE POINT INPUT")
    while (bytes.length < count)
      bytes += System.in.read().toByte
    new String(bytes.toArray).codePointAt(0)
  }

  private def segv(address: Long, message: String): Nothing = throw new SegmentationFault(address, instruction, relative, section, memory.buffer.toArray, message)

  //noinspection SameParameterValue
  private def debug(str: String): Unit = if (debug) { System.err.println(str) }
  private def debug(str: String, i: Int): Unit = if (debug) { System.err.println(str + i) }
  private def debug(str: String, i: Long): Unit = if (debug) { System.err.println(str + i) }
  
  private def check32(long: Long, error: String): Int = {
    if (long < Integer.MIN_VALUE || long > Integer.MAX_VALUE) {
      segv(long, error)
    } else {
      long.toInt
    }
  }
}

object IntCodeInterpreter {

  def create(in: DataInput, flagSet: Long, flagUnset: Long): IntCodeInterpreter = {
    val (flags, ints) = IntCodeIO.readBinary(in)
    new IntCodeInterpreter(new Memory(ListBuffer.from(ints)), (flags & (~flagUnset)) | flagSet)
  }
  
  def create(reader: BufferedReader, flags: Long): IntCodeInterpreter = {
    val ints = reader.lines().toArray.map(_.toString).filter(!_.isBlank).flatMap(_.split(",")).map(_.trim).map(_.toLong)
    new IntCodeInterpreter(new Memory(ListBuffer.from(ints)), flags)
  }
  
  def createBinary(path: Path, flagSet: Long, flagUnset: Long): IntCodeInterpreter = {
    val in = new DataInputStream(Files.newInputStream(path))
    val interpreter = create(in, flagSet, flagUnset)
    in.close()
    interpreter
  }
  
  //noinspection ScalaUnusedSymbol
  def createPlain(path: Path, flagSet: Long, flagUnset: Long): IntCodeInterpreter = {
    val in = Files.newBufferedReader(path)
    val interpreter = create(in, flagSet)
    in.close()
    interpreter
  }
}
