package extintcode

import joptsimple.util.EnumConverter

import java.util
import scala.reflect.{ClassTag, classTag}
import scala.jdk.CollectionConverters._

object Util {

  def exit(code: Int): Nothing = {
    System.exit(code)
    throw new Error("System.exit returned.")
  }
  
  def enumArg[T <: Enum[T] : ClassTag]: EnumConverter[T] = new ConcreteEnumConverter(classTag[T].runtimeClass.asInstanceOf[Class[T]])
}

class ConcreteEnumConverter[T <: Enum[T]](clazz: Class[T]) extends EnumConverter[T](clazz) {
  
  override def valuePattern(): String = {
    util.EnumSet.allOf(valueType()).asScala.map(_.name().toLowerCase).mkString("|")
  }
}
