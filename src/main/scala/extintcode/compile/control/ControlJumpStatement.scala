package extintcode.compile.control

import extintcode.asm.{AssemblyData, AssemblyText, StmtJmp}
import extintcode.compile.{CompilerRuntime, ImportTable, LangStatement}

class ControlJumpStatement(val statement: ControlJumpType) extends LangStatement {
  
  override def code(imports: ImportTable, runtime: CompilerRuntime): (List[AssemblyText], List[AssemblyData]) = {
    val either = statement match {
      case ControlJumpType.BREAK => runtime.getControl.break
      case ControlJumpType.CONTINUE => runtime.getControl.continue
      case ControlJumpType.NEXT => runtime.getControl.next
    }
    either match {
      case Left(v) => (List(StmtJmp(v)), Nil)
      case Right(e) => throw new IllegalStateException(e)
    }
  }
}
