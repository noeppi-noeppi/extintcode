package extintcode.asm

import extintcode.Util
import extintcode.util.IntCodeIO
import joptsimple.OptionParser
import joptsimple.util.{PathConverter, PathProperties}

import java.nio.file.{Files, Path, StandardOpenOption}
import scala.jdk.CollectionConverters._

object IntCodeAsm {

  def run(args: String*): Unit = {
    val options = new OptionParser(false)
    val specOutput = options.acceptsAll(List("o", "output").asJava, "The output file").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specHeader = options.acceptsAll(List("h", "header").asJava, "If this option is set, an IntCode Header will be generated as well.")
    val specHeaderOutput = options.acceptsAll(List("g", "header-out").asJava, "The output path of the generated header").availableIf(specHeader).withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specFile = options.nonOptions("The input file to read.").withValuesConvertedBy(new PathConverter(PathProperties.FILE_EXISTING, PathProperties.READABLE))
    val set = options.parse(args: _*)
    if (!set.has(specFile) || set.valueOf(specFile) == null) {
      println("No input file. Nothing to do.")
      println()
      options.printHelpOn(System.out)
      Util.exit(0)
    }
    val path = set.valueOf(specFile).toAbsolutePath.normalize()
    val moduleName = if (path.getFileName.toString.endsWith(".inta")) {
      path.getFileName.toString.dropRight(5)
    } else {
      println("Input file does not end on 'inta'. Assuming module name '" + path.getFileName.toString + "'.")
      path.getFileName.toString
    }
    val output = (if (set.has(specOutput) && set.valueOf(specOutput) != null) {
      set.valueOf(specOutput)
    } else {
      path.getParent.resolve(moduleName + ".intr")
    }).toAbsolutePath.normalize()
    val headerOutput: Option[Path] = (if (set.has(specHeader)) {
      Some(if (set.has(specHeaderOutput) && set.valueOf(specHeaderOutput) != null) {
        set.valueOf(specHeaderOutput)
      } else {
        output.getParent.resolve(moduleName + ".inth")
      })
    } else {
      None
    }).map(_.toAbsolutePath.normalize())
    val writer = headerOutput.map(Files.newBufferedWriter(_, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING))
    val module = IntCodeAssembler.assemble(moduleName, Files.newBufferedReader(path), writer)
    writer.foreach(_.close())
    IntCodeIO.writeRelocatable(Files.newBufferedWriter(output, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING), module)
  }
}
