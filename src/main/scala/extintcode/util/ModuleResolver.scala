package extintcode.util

import extintcode.Main

import java.io.{BufferedReader, InputStreamReader}
import java.nio.file.{Files, Path}

trait ModuleResolver[T <: Module] {

  def resolve(name: String): Option[T]
}

object ModuleResolver {
  
  lazy val builtinRelocatable: ModuleResolver[RelocatableModule] = new BuiltinModuleResolver("extintcode/lib", "intr", IntCodeIO.readRelocatable)
  lazy val builtinHeader: ModuleResolver[HeaderModule] = new BuiltinModuleResolver("extintcode/lib", "inth", IntCodeIO.readHeader)
  
  def of[T <: Module](parents: ModuleResolver[T]*) = new CompositeModuleResolver(parents)
  def path[T <: Module](base: Path, suffix: String, factory: (String, BufferedReader) => T) = new DirectoryModuleResolver(base, suffix, factory)
  def special[T <: Module](modules: (String, T)*) = new SpecialModuleResolver(modules.toMap)
}

class CompositeModuleResolver[T <: Module](private val parents: Seq[ModuleResolver[T]]) extends ModuleResolver[T] {
  
  override def resolve(name: String): Option[T] = parents.foldLeft[Option[T]](None)((value, parent) => value match {
    case Some(_) => value
    case None => parent.resolve(name)
  })
}

class DirectoryModuleResolver[T <: Module](private val base: Path, private val suffix: String, private val factory: (String, BufferedReader) => T) extends ModuleResolver[T] {
  
  override def resolve(name: String): Option[T] = Some(base.resolve(name + "." + suffix))
    .filter(Files.isRegularFile(_))
    .filter(Files.isReadable)
    .map(Files.newBufferedReader)
    .map(reader => {
      val x = factory(name, reader)
      reader.close()
      x
    })
}

class BuiltinModuleResolver[T <: Module](base: String, suffix: String, private val factory: (String, BufferedReader) => T) extends ModuleResolver[T] {
  
  override def resolve(name: String): Option[T] = Option(Main.getClass.getClassLoader.getResourceAsStream(base + "/" + name + "." + suffix))
    .map(new InputStreamReader(_))
    .map(new BufferedReader(_))
    .map(reader => {
      val x = factory(name, reader)
      reader.close()
      x
    })
}

class SpecialModuleResolver[T <: Module](private val map: Map[String, T]) extends ModuleResolver[T] {
  
  override def resolve(name: String): Option[T] = map.get(name)
}