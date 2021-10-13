package extintcode.compile.control

import extintcode.asm.{AssemblyData, AssemblyText, StmtJmp}
import extintcode.compile.{CodePathCheckResult, CodePathChecker, CompilerRuntime, ImportTable, LangStatement}

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

  override def checkCodePath(): CodePathCheckResult = {
    if (statement == ControlJumpType.CONTINUE || statement == ControlJumpType.NEXT) {
      // Jumping up in a loop will either create an infinite loop, which is not a problem of code path checking
      // or there is a branch somewhere else, so the loop ends. If that branch is a yes and the loop does not
      // run infinite, it will definitely return.
      // If that branch is a no and is combined with a yes, this will give a no.
      // So in either case for non-infinite loops we can return a yes if we jump up the loop
      CodePathChecker.yes()
    } else {
      // A break breaks code path checking. It is a never
      CodePathChecker.never(this)
    }
  }
}
