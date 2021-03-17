package extintcode.compile

import extintcode.Util
import extintcode.asm._
import extintcode.compile.function.FunctionDefinition
import extintcode.compile.meta.ImportStatement
import extintcode.compile.variable.VariableDeclaration
import extintcode.util._
import joptsimple.OptionParser
import joptsimple.util.{PathConverter, PathProperties}

import java.io.{BufferedWriter, File}
import java.nio.file.{Files, StandardOpenOption}
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

object IntCodeCompiler {
  
  def run(args: String*): Unit = {
    val options = new OptionParser(false)
    val specOutput = options.acceptsAll(List("o", "output").asJava, "The output file").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specHeader = options.acceptsAll(List("h", "header").asJava, "Search paths for headers.").withRequiredArg().withValuesSeparatedBy(File.pathSeparatorChar).withValuesConvertedBy(new PathConverter(PathProperties.DIRECTORY_EXISTING, PathProperties.READABLE))
    val specLib = options.acceptsAll(List("l", "lib", "library").asJava, "When this option is set, compilation will fail for any toplevel statement that is not a function or field declaration as those should not be used in libraries and may cause hard to find bugs.")
    val specFile = options.nonOptions("The input file to read.").withValuesConvertedBy(new PathConverter(PathProperties.FILE_EXISTING, PathProperties.READABLE))
    val set = options.parse(args: _*)    
    if (!set.has(specFile) || set.valueOf(specFile) == null) {
      println("No input file. Nothing to do.")
      println()
      options.printHelpOn(System.out)
      Util.exit(0)
    }
    val path = set.valueOf(specFile).toAbsolutePath.normalize()
    val moduleName = if (path.getFileName.toString.endsWith(".intx")) {
      path.getFileName.toString.dropRight(5)
    } else {
      println("Input file does not end on 'intx'. Assuming module name '" + path.getFileName.toString + "'.")
      path.getFileName.toString
    }
    val output = (if (set.has(specOutput) && set.valueOf(specOutput) != null) {
      set.valueOf(specOutput)
    } else {
      path.getParent.resolve(moduleName + ".inta")
    }).toAbsolutePath.normalize()
    val additionalHeaders: List[ModuleResolver[HeaderModule]] = set.valuesOf(specHeader).asScala
      .map(_.toAbsolutePath.normalize())
      .map(ModuleResolver.path(_, "inth", IntCodeIO.readHeader)).toList
    val resolver = ModuleResolver.of(ModuleResolver.builtinHeader :: additionalHeaders: _*)
    val reader = Files.newBufferedReader(path)
    println("Parsing Input")
    var line: String = null
    do {
      line = reader.readLine()
      if (line == null) throw new InvalidFileException("Missing version specifier.")
    } while (line.isBlank)
    val version = IntCodeIO.parseVersion(line)
    val program = ProgramParser.parseProgram(reader)
    reader.close()
    val writer = Files.newBufferedWriter(output, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    compile(moduleName, version, program, resolver, writer, set.has(specLib))
  }
  
  def compile(module: String, version: Int, program: List[Either[LangStatement, FunctionDefinition]], resolver: ModuleResolver[HeaderModule], writer: BufferedWriter, library: Boolean): Unit = {
    if (library) {
      println("Compiling library " + module)
    } else {
      println("Compiling program " + module)
    }
    val (statements, local) = program.partitionMap(identity)
    val runtime = new CompilerRuntime
    println("Building import table")
    val imports = new ImportTable(ModuleResolver.of(
      ModuleResolver.special(IntCodeRuntime.Modules.BUILTIN -> BuiltinHeader),
      resolver
    ), local)
    val text = ListBuffer[AssemblyText]()
    val data = ListBuffer[AssemblyData]()
    if (statements.isEmpty && local.isEmpty) {
      throw new IllegalStateException("Nothing to comile")
    }
    
    runtime.pushInitialScope()
    if (statements.nonEmpty) {
      println("Compiling static code")
      for (statement <- statements) {
        if (library && !validLibraryToplevel(statement)) throw new InvalidFileException("Invalid library: Statement of type " + statement.getClass.getSimpleName + " ist not allowed as toplevel staement in a library.")
        val (c, d) = statement.code(imports, runtime)
        text.addAll(c)
        data.addAll(d)
      }
    }
    if (local.nonEmpty) {
      println("Compiling functions")
      runtime.specialLabel("end")
      text.addOne(StmtJmp(DirectLabel("end")))
      for (function <- local) {
        val (c, d) = function.code(imports, runtime)
        text.addAll(c)
        data.addAll(d)
      }
      text.addOne(CodeLabel("end"))
    }
    data.addAll(runtime.getCollectedDataEntries)
    runtime.popScope()
    
    runtime.getPossiblyConstantVars.foreach(v => println("Variable " + v.name + " can be a constant."))
    
    println("Writing IntCode Assembler")
    writer.write("$V " + IntCode.MAJOR + "." + version + "\n")
    imports.getDependencies.foreach(dep => writer.write("$D " + dep._1 + "." + IntCode.MAJOR + "." + dep._2 + "\n"))
    writer.write("\n")
    writer.write("section .text\n")
    text.foreach(line => writer.write(line.textStr() + "\n"))
    if (data.nonEmpty) {
      writer.write("\n")
      writer.write("section .data\n")
      data.foreach(line => writer.write(line.dataStr() + "\n"))
    }
    writer.close()
  }
  
  def validLibraryToplevel(statement: LangStatement): Boolean = statement match {
    case _: ImportStatement => true
    case x: VariableDeclaration if x.exported => true
    case _ => false
  }
}
