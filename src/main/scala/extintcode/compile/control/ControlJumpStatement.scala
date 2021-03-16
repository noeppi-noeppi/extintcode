package extintcode.compile.control

import extintcode.asm.{AssemblyData, AssemblyText, StmtJmp}
import extintcode.compile.{CompilerRuntime, ImportTable, LangStatement}

class ControlJumpStatement(val statement: ControlJumpType) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    statement match {
      case ControlJumpType.BREAK => (List(StmtJmp(runtime.getControl.break)), Nil)
      case ControlJumpType.CONTINUE => (List(StmtJmp(runtime.getControl.continue)), Nil)
      case ControlJumpType.NEXT => (List(StmtJmp(runtime.getControl.next)), Nil)
    }
  }
}
