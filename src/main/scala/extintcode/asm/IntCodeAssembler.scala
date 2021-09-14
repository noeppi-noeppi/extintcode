package extintcode.asm

import extintcode.util.{FieldEntry, FunctionEntry, IntCode, IntCodeIO, IntCodeRuntime, InvalidFileException, RelocatableModule}

import java.io.{BufferedReader, Writer}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object IntCodeAssembler {
  
  private val R_TEXT = """^\s*section\s+\.text\s*$""".r
  private val R_DATA = """^\s*section\s+\.data\s*$""".r
  private val R_PARAM = """^PARAM(\d+)$""".r
  
  val NEXTDYN: ValType = Memory(IntCodeRuntime.NEXTDYN, null)
  val CALLSTACK: ValType = Memory(IntCodeRuntime.CALLSTACK, null)
  val BACKJUMP: ValType = Memory(IntCodeRuntime.BACKJUMP, null)
  val RETURN: ValType = Memory(IntCodeRuntime.RETURN, null)
  val X1: ValType = Memory(IntCodeRuntime.X1, null)
  val X2: ValType = Memory(IntCodeRuntime.X2, null)
  val GLOBAL1: ValType = Memory(IntCodeRuntime.GLOBAL1, null)
  val GLOBAL2: ValType = Memory(IntCodeRuntime.GLOBAL2, null)
  val GLOBAL3: ValType = Memory(IntCodeRuntime.GLOBAL3, null)
  val GLOBAL4: ValType = Memory(IntCodeRuntime.GLOBAL4, null)
  val GLOBAL5: ValType = Memory(IntCodeRuntime.GLOBAL5, null)
  val GLOBAL6: ValType = Memory(IntCodeRuntime.GLOBAL6, null)
  val GLOBAL7: ValType = Memory(IntCodeRuntime.GLOBAL7, null)
  val GLOBAL8: ValType = Memory(IntCodeRuntime.GLOBAL8, null)
  
  def assemble(name: String, reader: BufferedReader, headerOut: Option[Writer]): RelocatableModule = {
    val lines = reader.lines().toArray.map(_.toString).filter(!_.isBlank)
    if (lines.isEmpty) throw new InvalidFileException("Empty file")
    val version = IntCodeIO.parseVersion(lines.head)
    val dependencyLines = ListBuffer[String]()
    val textLines = ListBuffer[String]()
    val dataLines = ListBuffer[String]()
    var mode = 0
    for (line <- lines.tail) {
      mode match {
        case 0 if R_TEXT.matches(line) => mode = 1
        case 0 => dependencyLines.addOne(line)
        case 1 if R_DATA.matches(line) => mode = 2
        case 1 => textLines.addOne(line)
        case 2 => dataLines.addOne(line)
      }
    }
    if (mode == 0) throw new InvalidFileException("Text section missing in IntCode Assembler")
    val dependencies = mutable.Map[String, Int]()
    dependencyLines.foreach {
      case IntCodeIO.R_DEPENDENCY(name, major, _) if major.toInt == IntCode.MAJOR && dependencies.contains(name) => throw new InvalidFileException("Duplicate declaration of dependency: " + name)
      case IntCodeIO.R_DEPENDENCY(name, major, value) if major.toInt == IntCode.MAJOR => dependencies(name) = value.toInt
      case IntCodeIO.R_DEPENDENCY(_, major, _) => throw new InvalidFileException("IntCode Assembly has a dependency with a differen major version: " + major + " (Current major version is " + IntCode.MAJOR + ")")
    }
    val text = textLines.toList.flatMap(AssemblyParser.parseTextLine)
    val data = dataLines.toList.flatMap(AssemblyParser.parseDataLine)
    val labelMap = mutable.Map[String, Int]()
    val dataMap = mutable.Map[String, Int]()
    val fieldMap = mutable.Map[FieldEntry, Long]()
    val functionMap = mutable.Map[FunctionEntry, Long]()
    var inst = 0
    var maxFunc = 0
    text.foreach {
      case Left(x) =>
        x.printWarning()
        inst += x.size
      case Right(x) =>
        x.name.foreach(labelMap.put(_, inst))
        x.function.foreach(functionMap.put(_, inst))
        x.function.foreach(elem => maxFunc = Math.max(elem.args, maxFunc))
        x.field.foreach(label => throw new InvalidFileException("Field labels are not allowed in text section: " + label))
    }
    inst += 3 // Jump over the data segment
    data.foreach {
      case Left(x) =>
        dataMap(x.name) = inst
        inst += x.size
      case Right(x) =>
        x.name.foreach(label => throw new InvalidFileException("Named labels are not allowed in data section: " + label))
        x.function.foreach(label => throw new InvalidFileException("Function labels are not allowed in data section: " + label))
        x.field.foreach(fieldMap.put(_, inst))
    }
    val labels = labelMap.toMap
    val dataEntries = dataMap.toMap
    val ints = ListBuffer[(Long, String)]()
    val dataSegmentEnd = inst
    inst = 0
    text.foreach {
      case Left(x) =>
        val code = x.code(LabelData(inst, maxFunc, dependencies.nonEmpty, labels, dataEntries))
        if (code.size != x.size) throw new IllegalStateException("Internal Assembler error: Statement wrote invalid amount of ints: Expected: " + x.size + ", Written: " + code.size)
        ints.addAll(code)
        inst += x.size
      case Right(_) =>
    }
    ints.addAll(Seq(
      (1106, null), (0, null), (dataSegmentEnd, "")
    ))
    data.foreach {
      case Left(x) =>
        val code = x.code(dataEntries)
        if (code.size != x.size) throw new IllegalStateException("Internal Assembler error: Data Entry wrote invalid amount of ints: Expected: " + x.size + ", Written: " + code.size)
        ints.addAll(code)
      case Right(_) =>
    }
    headerOut.foreach(writer => {
      writer.write("$V " + IntCode.MAJOR + "." + version + "\n")
      fieldMap.toList.sortBy(_._2).foreach(field => {
        writer.write(field._1.toStringWithAddress(field._2) + "\n")
      })
      functionMap.toList.sortBy(_._2).foreach(func => {
        writer.write(func._1.toStringWithAddress(func._2) + "\n")
      })
    })
    new RelocatableModule(name, version, dependencies.toMap, maxFunc, ints.toList)
  }
  
  def parseSpecialValue(value: String, maxFunc: Int): ValType = value.trim match {
    case IntCodeRuntime.Names.NEXTDYN => NEXTDYN
    case IntCodeRuntime.Names.CALLSTACK => CALLSTACK
    case IntCodeRuntime.Names.BACKJUMP => BACKJUMP
    case IntCodeRuntime.Names.RETURN => RETURN
    case IntCodeRuntime.Names.GLOBAL1 => GLOBAL1
    case IntCodeRuntime.Names.GLOBAL2 => GLOBAL2
    case IntCodeRuntime.Names.GLOBAL3 => GLOBAL3
    case IntCodeRuntime.Names.GLOBAL4 => GLOBAL4
    case IntCodeRuntime.Names.GLOBAL5 => GLOBAL5
    case IntCodeRuntime.Names.GLOBAL6 => GLOBAL6
    case IntCodeRuntime.Names.GLOBAL7 => GLOBAL7
    case IntCodeRuntime.Names.GLOBAL8 => GLOBAL8
    case R_PARAM(num) if num.toInt > 0 && num.toInt <= maxFunc => Memory(IntCodeRuntime.PARAM_BASE + num.toInt - 1, null)
    case R_PARAM(num) => throw new InvalidFileException("Parameter " + num + " can not be accessed: Maximal param number is " + maxFunc)
  }
  
  def opcode(op: Int, params: ValType*): (Long, String) = {
    var x: Long = op
    for (i <- 0 until params.size) {
      x += (Math.pow(10, 2 + i).toLong * params(i).mode)
    }
    (x, null)
  }
  
  def stringToCodePoints(string: String): Seq[Long] = string.codePoints().toArray.toIndexedSeq.map(_.toLong)

  def paramName0Based(num: Int): String = "PARAM" + (num + 1)
}
