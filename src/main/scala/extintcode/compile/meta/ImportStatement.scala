package extintcode.compile.meta

import extintcode.asm.{AssemblyData, AssemblyText}
import extintcode.compile.{CompilerRuntime, ImportTable, LangStatement}

class ImportStatement(val statement: String, val isImplicit: Boolean) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    if (statement.contains(".")) {
      val module = statement.substring(0, statement.indexOf('.')).strip()
      val name = statement.substring(statement.indexOf('.') + 1).strip()
      val names = if (name == "*") {
        if (isImplicit) {
          imports.getFunctionNames(module)
        } else {
          imports.getNames(module)
        }
      } else {
        Set(name)
      }
      if (isImplicit) {
        names.foreach(imports.importImplicit(module, _))
      } else {
        names.foreach(imports.importName(module, _))
      }
    } else if (isImplicit) {
      throw new IllegalStateException("Can't implicit import module '" + statement.strip() + "'. Use 'implicit " + statement.strip() + ".*' instead.")
    } else {
      imports.importModule(statement.strip())
    }
    (Nil, Nil)
  }
}
