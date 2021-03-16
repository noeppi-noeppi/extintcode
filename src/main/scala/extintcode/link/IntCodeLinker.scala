package extintcode.link

import extintcode.Util
import extintcode.util._
import extintcode.vm.IntCodeFormat
import joptsimple.OptionParser
import joptsimple.util.{PathConverter, PathProperties}

import java.io.{DataOutputStream, File}
import java.nio.file.{Files, StandardOpenOption}
import scala.jdk.CollectionConverters._

object IntCodeLinker {
  
  val DEFAULT_FLAGS: Long = IntCodeFlags.ASCII
  
  def run(args: String*): Unit = {
    val options = new OptionParser(false)
    val specFormat = options.acceptsAll(List("f", "format").asJava, "The output format").withRequiredArg().withValuesConvertedBy(Util.enum[IntCodeFormat]).defaultsTo(IntCodeFormat.BINARY)
    val specEnable = options.acceptsAll(List("e", "enable").asJava, "Enable a flag. This has no effect when output format is plain.").withRequiredArg().withValuesSeparatedBy(',').withValuesConvertedBy(Util.enum[IntCodeFlagEnum])
    val specDisable = options.acceptsAll(List("d", "disable").asJava, "Disable a flag. This has no effect when output format is plain.").withRequiredArg().withValuesSeparatedBy(',').withValuesConvertedBy(Util.enum[IntCodeFlagEnum])
    val specOutput = options.acceptsAll(List("o", "output").asJava, "The output file").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specStack = options.acceptsAll(List("s", "stack").asJava, "The size of the call stack to allocate").withRequiredArg().ofType(classOf[Integer]).defaultsTo(1000)
    val specLib = options.acceptsAll(List("l", "lib", "library").asJava, "Search paths for libraries.").withRequiredArg().withValuesSeparatedBy(File.pathSeparatorChar).withValuesConvertedBy(new PathConverter(PathProperties.DIRECTORY_EXISTING, PathProperties.READABLE))
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
    val flags = (DEFAULT_FLAGS & (~flagUnset)) | flagSet
    val format = set.valueOf(specFormat)
    val path = set.valueOf(specFile).toAbsolutePath.normalize()
    val moduleName = if (path.getFileName.toString.endsWith(".intr")) {
      path.getFileName.toString.dropRight(5)
    } else {
      println("Input file does not end on 'intr'. Assuming module name '" + path.getFileName.toString + "'.")
      path.getFileName.toString
    }
    val output = (if (set.has(specOutput) && set.valueOf(specOutput) != null) {
      set.valueOf(specOutput)
    } else {
      path.getParent.resolve(moduleName + "." + format.extension)
    }).toAbsolutePath.normalize()
    val stack = set.valueOf(specStack)
    val additionalLibs: List[ModuleResolver[RelocatableModule]] = set.valuesOf(specLib).asScala
      .map(_.toAbsolutePath.normalize())
      .map(ModuleResolver.path(_, "intr", IntCodeIO.readRelocatable)).toList
    val resolver = ModuleResolver.of(ModuleResolver.builtinRelocatable :: additionalLibs: _*)
    val reader = Files.newBufferedReader(path)
    val module = IntCodeIO.readRelocatable(moduleName, reader)
    reader.close()
    val ints = IntCodeRelocator.relocate(module, resolver, stack)
    if (format == IntCodeFormat.PLAIN) {
      val writer = Files.newBufferedWriter(output, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      writer.write(ints.mkString("", ",", "\n"))
      writer.close()
    } else {
      val out = new DataOutputStream(Files.newOutputStream(output, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING))
      IntCodeIO.writeBinary(out, flags, ints)
      out.close()
    }
  }
}
