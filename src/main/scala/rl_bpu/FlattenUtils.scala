package rl_bpu

import chisel3._
import chisel3.util._
import chisel3.experimental.{ChiselAnnotation, RunFirrtlTransform}
import firrtl.transforms.{NoDedupAnnotation, FlattenAnnotation, Flatten}
import firrtl.annotations.{Annotation}
import firrtl.{Transform}

// Can flatten in two ways
// 1: Add to Module definition
//
// import chisel3.util.experimental.{FlattenInstance}
// class ModuleX with FlattenInstance
//
// 2: Add to the instance
// val mod = Module(new ModuleX)
// FlattenInst(mod)

object FlattenInst
{
  def apply[T <: Module](module: T): Unit = {
     Seq(
       new ChiselAnnotation with RunFirrtlTransform {
         def toFirrtl: Annotation = FlattenAnnotation(module.toNamed)
         def transformClass: Class[_ <: Transform] = classOf[Flatten]
       },
       new ChiselAnnotation {
         def toFirrtl: Annotation = NoDedupAnnotation(module.toNamed)
       }
     )
     .map(chisel3.experimental.annotate(_))
  }
}

