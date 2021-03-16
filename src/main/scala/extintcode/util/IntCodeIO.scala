package extintcode.util

import java.io.{BufferedReader, DataInput, DataOutput, Writer}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

object IntCodeIO {
  
  val R_VERSION: Regex = """^\s*\$\s*V\s*(\d+)\s*\.\s*(\d+)\s*$""".r
  val R_FUNC: Regex = """^\s*\$\s*F\s*(\d+)\s*$""".r
  val R_DEPENDENCY: Regex = """^\s*\$\s*D\s*([A-Za-z][A-Za-z_0-9]*)\s*\.\s*(\d+)\s*\.\s*(\d+)\s*$""".r
  val R_RELOCATE_SELF: Regex = """^\s*%\s*(-?\d+)\s*$""".r
  val R_RELOCATE: Regex = """^\s*([A-Za-z][A-Za-z_0-9]*)\s*%\s*(-?\d+)\s*$""".r
  val R_NUMBER: Regex = """^\s*(-?\d+)\s*$""".r
  
  def readBinary(in: DataInput): (Long, Seq[Long]) = {
    if (in.readLong() != IntCode.MAGIC) {
      throw new InvalidFileException("Assembled IntCode file is invalid: Magic value not present")
    }
    if (in.readUnsignedShort() != IntCode.MAJOR) {
      throw new InvalidFileException("Assembled IntCode file can not be parsed: Major version mismatch.")
    }
    val flags = in.readLong()
    val ints = ListBuffer[Long]()
    var code = in.readUnsignedByte()
    while (code != 255) {
      code match {
        case 0 => ints.addOne(0)
        case 1 => ints.addOne(1)
        case 2 => ints.addOne(2)
        case 3 => ints.addOne(3)
        case 4 => ints.addOne(4)
        case 5 => ints.addOne(5)
        case 6 => ints.addOne(10)
        case 7 => ints.addOne(-1)
        case 8 => ints.addOne(in.readByte())
        case 9 => ints.addAll(Seq.fill(in.readUnsignedShort())(0))
        case 16 => ints.addOne(in.readShort())
        case 32 => ints.addOne(in.readInt())
        case 64 => ints.addOne(in.readLong())
      }
      code = in.readUnsignedByte()
    }
    (flags, ints.toSeq)
  }
  
  def writeBinary(out: DataOutput, flags: Long, ints: Seq[Long]): Unit = {
    out.writeLong(IntCode.MAGIC)
    out.writeShort(IntCode.MAJOR)
    out.writeLong(flags)
    var zeros = 0
    ints.foreach(int => {
      if ((int != 0 || zeros == ((1 << 16) - 1)) && zeros > 0) {
        if (zeros == 1) {
          out.writeByte(0)
        } else {
          out.writeByte(9)
          out.writeShort(zeros)
        }
        zeros = 0
      }
      int match {
        case 0 => zeros += 1
        case 1 => out.writeByte(1)
        case 2 => out.writeByte(2)
        case 3 => out.writeByte(3)
        case 4 => out.writeByte(4)
        case 5 => out.writeByte(5)
        case 10 => out.writeByte(6)
        case -1 => out.writeByte(7)
        case x if x >= Byte.MinValue && x <= Byte.MaxValue =>
          out.writeByte(8)
          out.writeByte(x.toByte)
        case x if x >= Short.MinValue && x <= Short.MaxValue =>
          out.writeByte(16)
          out.writeShort(x.toShort)
        case x if x >= Int.MinValue && x <= Int.MaxValue =>
          out.writeByte(32)
          out.writeInt(x.toInt)
        case x =>
          out.writeByte(64)
          out.writeLong(x)
      }
    })
    out.writeByte(255)
  }
  
  def readRelocatable(name: String, reader: BufferedReader): RelocatableModule = {
    var line: String = null
    do {
      line = reader.readLine()
      if (line == null) throw new InvalidFileException("Unexpected end of file: IntCode Relocatable")
    } while (line.isBlank)
    val version = parseVersion(line)
    line = reader.readLine()
    var meta = true
    var maxFunc = -1
    val dependencies = mutable.Map[String, Int]()
    val code = ListBuffer[(Long, String)]()
    while (line != null) {
      if (!line.isBlank) {
        if (line.contains("|")) {
          line = line.substring(0, line.indexOf('|'))
        }
        line match {
          case R_FUNC(x) if meta && maxFunc < 0 => maxFunc = x.toInt
          case R_FUNC(_) if meta => throw new InvalidFileException("Duplicate definition of maximum function parameters.")
          case R_DEPENDENCY(name, major, _) if meta && major.toInt == IntCode.MAJOR && dependencies.contains(name) => throw new InvalidFileException("Duplicate declaration of dependency: " + name)
          case R_DEPENDENCY(name, major, value) if meta && major.toInt == IntCode.MAJOR => dependencies(name) = value.toInt
          case R_DEPENDENCY(_, major, _) if meta => throw new InvalidFileException("IntCode relocatable has a dependency with a differen major version: " + major + " (Current major version is " + IntCode.MAJOR + ")")
          case _ =>
            meta = false
            code.addAll(line.split(",").map {
              case R_RELOCATE_SELF(number) => (number.toLong, "")
              case R_RELOCATE(module, number) => (number.toLong, module)
              case R_NUMBER(number) => (number.toLong, null)
            })
        }
      }
      line = reader.readLine()
    }
    if (maxFunc < 0) {
      println("Warning: Reading relocatable IntCode: Max Func Params value was not set. Assuming 0.")
      maxFunc = 0
    }
    new RelocatableModule(name, version, dependencies.toMap, maxFunc, code.toSeq)
  }
  
  def writeRelocatable(writer: Writer, module: RelocatableModule): Unit = {
    writer.write("$V " + IntCode.MAJOR + "." + module.version + "\n")
    writer.write("$F " + module.maxFunc + "\n")
    writer.write(module.dependencies.map(entry => "$D " + entry._1 + "." + IntCode.MAJOR + "." + entry._2 + "\n").mkString(""))
    writer.write(module.code.map {
      case (number, null) => number.toString
      case (number, module.name) => "%" + number.toString
      case (number, m) => m + "%" + number
    }.mkString("", ",", "\n"))
    writer.flush()
  }
  
  def readHeader(name: String, reader: BufferedReader): HeaderModule = {
    var line: String = null
    do {
      line = reader.readLine()
      if (line == null) throw new InvalidFileException("Unexpected end of file: IntCode Header")
    } while (line.isBlank)
    val version = parseVersion(line)
    line = reader.readLine()
    val fields = mutable.Map[String, (FieldEntry, Long)]()
    val funcs = mutable.Map[(String, Int), (FunctionEntry, Long)]()
    while (line != null) {
      if (!line.isBlank) {
        if (line.contains("#")) {
          line = line.substring(0, line.indexOf('|'))
        }
        parseFieldOrFunc(line.strip()) match {
          case Left((field, _)) if fields.contains(field.name) => throw new InvalidFileException("Duplicate field definition in header: '" + field.name + "' ")
          case Left((field, address)) => fields.put(field.name, (field, address))
          case Right((function, _)) if funcs.contains((function.name, function.args)) => throw new InvalidFileException("Duplicate function definition in header: '" + function.name + "#" + function.args + "' ")
          case Right((function, address)) => funcs.put((function.name, function.args), (function, address))
        }
      }
      line = reader.readLine()
    }
    new HeaderModule(name, version, fields.values.toList, funcs.values.toList)
  }
  
  private def parseFieldOrFunc(entry: String): Either[(FieldEntry, Long), (FunctionEntry, Long)] = entry match {
    case FieldEntry.REGEX_WA(_*) => Left(FieldEntry.parseWithAddress(entry))
    case FunctionEntry.REGEX_WA(_*) => Right(FunctionEntry.parseWithAddress(entry))
    case _ => throw new InvalidFileException("Invalid field or function signature: " + entry)
  }
  
  def parseVersion(string: String): Int = string match {
    case R_VERSION(major, version) if major.toShort == IntCode.MAJOR => version.toInt
    case R_VERSION(major, _) => throw new InvalidFileException("File was created for a different major version. This compiler is on version " + IntCode.MAJOR + " but the file is on " + major + ".")
    case x => throw new InvalidFileException("File has invalid version specifier: '" + x + "'")
  }
}
