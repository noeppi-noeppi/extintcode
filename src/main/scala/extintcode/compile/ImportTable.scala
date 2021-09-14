package extintcode.compile

import extintcode.asm.{Direct, DirectLabel, ValType}
import extintcode.compile.function.FunctionDefinition
import extintcode.util.{FieldEntry, FunctionEntry, HeaderModule, IntCodeRuntime, ModuleResolver}

import scala.collection.mutable

class ImportTable(headers: ModuleResolver[HeaderModule], localFunctions: List[FunctionDefinition]) {
  
  localFunctions
    .map(func => (func.name, func.paramAmount))
    .groupBy(identity)
    .collect { case (x, List(_,_,_*)) => x }
    .foreach(dup =>
      throw new IllegalStateException("Failed to create import table: Duplicate function declaration: " + dup._1 + "#" + dup._2)
    )
  
  private val headerMap = mutable.Map[String, HeaderModule]()
  private val localMap: Map[(String, Int), (FunctionEntry, ValType)] = localFunctions
    .map(func => (func.name, func.paramAmount) -> (func.entry, DirectLabel(func.label)))
    .toMap
  private val importedModules = mutable.Set[String]()
  private val importedNames = mutable.Map[String, String]()
  private val implicitImports = mutable.Map[(String, String, Int), (FunctionEntry, ValType)]()
  private val dependencies = mutable.Map[String, Int]()
  
  private def updateHeaderMap(module: String): Unit = {
    if (!headerMap.contains(module) && module != IntCodeRuntime.Modules.LOCAL) {
      headers.resolve(module).foreach(header => {
        headerMap.put(header.name, header)
      })
    }
  }
  
  private def addDependency(module: String): Unit = {
    updateHeaderMap(module)
    if (!dependencies.contains(module) && module != IntCodeRuntime.Modules.LOCAL
      && module != IntCodeRuntime.Modules.BUILTIN) {
      if (!headerMap.contains(module)) throw new IllegalStateException("Internal compiler error: Can'T add unknown module to dependencies: " + module)
      dependencies.put(module, headerMap(module).version)
    }
  }
  
  def importModule(module: String): Unit = {
    updateHeaderMap(module)
    if (importedModules.contains(module)) {
      println("Warning: Duplicate import of module " + module)
    } else if (!headerMap.contains(module)) {
      throw new NoSuchElementException("Module " + module + " not found. Make sure the header is available.")
    } else {
      importedModules.add(module)
      addDependency(module)
    }
  }
  
  def importName(module: String, name: String): Unit = {
    updateHeaderMap(module)
    if (!headerMap.contains(module)) {
      throw new NoSuchElementException("Module " + module + " not found. Make sure the header is available.")
    } else {
      headerMap(module).checkName(name)
      importedNames.put(name, module)
      addDependency(module)
    }
  }
  
  def importImplicit(module: String, name: String): Unit = {
    updateHeaderMap(module)
    if (!module.startsWith("std")) {
      println("Warning: Implicit import from module not starting with std. This is probably not what you want.")
    }
    if (!headerMap.contains(module)) {
      throw new IllegalStateException("Can't implicit import " + module + "." + name + ": Module not found.")
    }
    val functions = headerMap(module).functions.filter(_._1._1 == name)
    if (functions.isEmpty) {
      throw new IllegalStateException("Can't implicit import " + module + "." + name + ": No matching function.")
    }
    functions.foreach(elem => {
      implicitImports.put((module, elem._1._1, elem._1._2), (elem._2._1, Direct(elem._2._2, module)))
    })
    addDependency(module)
  }
  
  def getImplicit(module: String, name: String, params: Int, feature: String): (String, FunctionEntry, ValType) = {
    val result = implicitImports.getOrElse((module, name, params), throw new IllegalStateException(feature + " requires implicit import of function " + module + "." + name + " which is not imported."))
    (module, result._1, result._2)
  }
  
  def isNameImported(name: String): Boolean = importedNames.contains(name)
  
  def getField(module: Option[String], name: String): (FieldEntry, ValType) = {
    val moduleName = module.map(x => {
      if (!importedModules.contains(x)) {
        throw new IllegalStateException("Can not access field " + name + ": Module " + x + " is not imported.")
      }
      x
    }).getOrElse({
      if (!importedNames.contains(name)) {
        throw new IllegalStateException("Name " + name + " not found. It is not imported.")
      }
      importedNames(name)
    })
    val entry = headerMap(moduleName).getField(name)
    (entry._1, Direct(entry._2, moduleName))
  }
  
  def getFunc(module: Option[String], name: String, params: Int): (String, FunctionEntry, ValType) = {
    val moduleName = module.map(x => {
      if (x != IntCodeRuntime.Modules.LOCAL && !importedModules.contains(x)) {
        throw new IllegalStateException("Can not access function " + name + ": Module " + x + " is not imported.")
      }
      x
    }).getOrElse({
      if (localMap.contains(name, params)) {
        IntCodeRuntime.Modules.LOCAL
      } else {
        if (!importedNames.contains(name)) {
          throw new IllegalStateException("Name " + name + " not found. It is not imported.")
        }
        importedNames(name)
      }
    })
    val result = if (moduleName == IntCodeRuntime.Modules.LOCAL) {
      if (!localMap.contains((name, params))) {
        throw new IllegalStateException("Local function " + name + "#" + params + " not defined.")
      }
      localMap((name, params))
    } else {
      val entry = headerMap(moduleName).getFunc(name, params)
      (entry._1, Direct(entry._2, moduleName))
    }
    (moduleName, result._1, result._2)
  }
  
  def getDependencies: Map[String, Int] = dependencies.toMap
  
  def getNames(module: String): Set[String] = {
    updateHeaderMap(module)
    if (module == IntCodeRuntime.Modules.LOCAL) {
      localMap.map(_._1._1).toSet
    } else if (!headerMap.contains(module)) {
      throw new NoSuchElementException("Module " + module + " not found. Make sure the header is available.")
    } else {
      headerMap(module).names
    }
  }
  
  def getFunctionNames(module: String): Set[String] = {
    updateHeaderMap(module)
    if (module == IntCodeRuntime.Modules.LOCAL) {
      localMap.map(_._1._1).toSet
    } else if (!headerMap.contains(module)) {
      throw new NoSuchElementException("Module " + module + " not found. Make sure the header is available.")
    } else {
      headerMap(module).functions.keySet.map(_._1)
    }
  }
}
