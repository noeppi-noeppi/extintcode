package extintcode.vm

import extintcode.Util
import extintcode.util.IntCodeFlagEnum
import joptsimple.OptionParser
import joptsimple.util.{PathConverter, PathProperties}

import java.nio.file.Files
import scala.jdk.CollectionConverters._

object IntCodeVM {
  
  def run(args: String*): Unit = {
    val options = new OptionParser(false)
    val specFormat = options.acceptsAll(List("f", "format").asJava, "The input format. Can be omitted for auto detection.").withRequiredArg().withValuesConvertedBy(Util.enum[IntCodeFormat])
    val specEnable = options.acceptsAll(List("e", "enable").asJava, "Enable a flag. This overrides values read from the input file").withRequiredArg().withValuesSeparatedBy(',').withValuesConvertedBy(Util.enum[IntCodeFlagEnum])
    val specDisable = options.acceptsAll(List("d", "disable").asJava, "Disable a flag. This overrides values read from the input file").withRequiredArg().withValuesSeparatedBy(',').withValuesConvertedBy(Util.enum[IntCodeFlagEnum])
    val specFile = options.nonOptions("The input file to read.").withValuesConvertedBy(new PathConverter(PathProperties.FILE_EXISTING, PathProperties.READABLE))
    val set = options.parse(args: _*)
    if (!set.has(specFile) || set.valueOf(specFile) == null) {
      println("No input file. Nothing to do.")
      println()
      options.printHelpOn(System.out)
      Util.exit(0)
    }
    var flagSet: Long = 0
    var flagUnset: Long = 0
    set.valuesOf(specEnable).asScala.foreach(flagSet |= _.flag)
    set.valuesOf(specDisable).asScala.foreach(flagUnset |= _.flag)
    val path = set.valueOf(specFile).toAbsolutePath.normalize()
    val format = if (set.has(specFormat)) {
      set.valueOf(specFormat)
    } else if (path.getFileName.toString.endsWith(".ic")) {
      IntCodeFormat.BINARY
    } else if (path.getFileName.toString.endsWith(".ints")) {
      IntCodeFormat.PLAIN
    } else {
      println("Could not infer input file type. Use --format to set the format.")
      Util.exit(1)
    }
    val interpreter = format.createInterpreter(path, flagSet, flagUnset)
    try {
      interpreter.run()
    } catch {
      case segv: SegmentationFault =>
        println("Segmentation fault: " + segv.getMessage)
        println("  At: " + segv.accessTry)
        println("  The instruction pointer was located at " + segv.instruction)
        println("  The relative base pointer was located at " + segv.relative)
        println("  The section address was located at " + segv.section)
        val coreDump = Files.createTempFile("core-intcode", "dump")
        println("Writing Core-Dump to " + coreDump.toAbsolutePath.normalize.toString)
        val out = Files.newBufferedWriter(coreDump)
        for (i <- 0 to (segv.memory.length + 1)) {
          if (i != 0) out.write(",")
          if (i < segv.memory.length) out.write(segv.memory(i).toString)
          else out.write("0")
          if (i == segv.accessTry) out.write(" segv")
          if (i == segv.instruction) out.write(" inst")
          if (i == segv.relative) out.write(" rel")
          if (i == segv.section) out.write(" sect")
        }
        out.close()
        System.err.println("Done")
    }
  }
}
