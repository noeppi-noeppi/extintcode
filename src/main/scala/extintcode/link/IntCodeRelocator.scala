package extintcode.link

import extintcode.util.{ModuleResolutionException, ModuleResolver, RelocatableModule}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object IntCodeRelocator {
  
  def relocate(main: RelocatableModule, resolver: ModuleResolver[RelocatableModule], callstack: Int): Seq[Long] = {
    val modules = ListBuffer[RelocatableModule]()
    val nv = mutable.Map[String, Int]()
    modules.addOne(main)
    nv.put(main.name, main.version)
    collectDependencies(main, resolver, module => {
      if (nv.getOrElse(module.name, module.version) != module.version) {
        throw new ModuleResolutionException("Different modules require different versions of module: " + module.name + " Versions: [" + module.version + ", " + nv.getOrElse(module.name, module.version) + "]", module.name, None)
      } else {
        nv(module.name) = module.version
        modules.addOne(module)
      }
    })
    relocate(modules.toSeq.reverse, callstack)
  }
  
  def relocate(modules: Seq[RelocatableModule], callstack: Int): Seq[Long] = {
    val maxFunc = modules.map(_.maxFunc).max
    val headerSize: Long = 32 + maxFunc + callstack
    val moduleMemory = mutable.Map[String, Long]()
    var nextDyn = headerSize
    modules.foreach(module => {
      moduleMemory(module.name) = nextDyn
      nextDyn += module.code.size
    })
    nextDyn += 1
    val ints = mutable.ListBuffer[Long]()
    ints.addAll(Seq(
      109, 32 + maxFunc,
      1106,0,headerSize,
      0,0,0,0,0,0,0,0,0,0,0
    ))
    ints.addAll(Seq(
      nextDyn,          // NEXTDYN
      32 + maxFunc,     // CALLSTACK
      0,                // BACKJUMP
      0,                // RETURN
      0,0,0,0,          // RESERVED
      0,0,0,0,0,0,0,0   // GLOBAL*
    ))
    ints.addAll(Seq.fill(maxFunc)(0))   // PARAM-MEMORY
    ints.addAll(Seq.fill(callstack)(0)) // CALLSTACK-MEMORY
    modules.foreach(module => {
      ints.addAll(module.code.map(entry => {
        if (entry._2 == null) {
          entry._1
        } else  {
          entry._1 + moduleMemory.getOrElse(if (entry._2 == "") module.name else entry._2, throw new IllegalStateException("Relocatable code contais relocation relative to unknown module: " + entry._2 + " (Code is in module: " + module.name + ")"))
        }
      }))
    })
    ints.addOne(99)
    ints.toSeq
  }
  
  private def collectDependencies(module: RelocatableModule, resolver: ModuleResolver[RelocatableModule], action: RelocatableModule => Unit): Unit = {
    module.dependencies
      .map(entry => (
        resolver.resolve(entry._1)
          .getOrElse(throw new ModuleResolutionException("Could not find module " + entry._1, entry._1, Some(module.name))),
        entry._2)
      ).map(entry => {
      if (entry._1.version != entry._2) {
        throw new ModuleResolutionException("Module version mismatch: Required: " + entry._2 + " Actual: " + entry._1.version, entry._1.name, Some(module.name))
      } else {
        entry._1
      }
    }).foreach(entry => {
      action(entry)
      collectDependencies(entry, resolver, action)
    })
  }
}
