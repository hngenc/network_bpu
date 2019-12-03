
package rl_bpu

import chisel3._
import chisel3.util.ShiftRegister
import hardfloat._

object VectorScalarMul {
  def apply(fp: FloatParams, v: Seq[UInt], s: UInt, latency: Int = 0): Seq[UInt] = {
    import fp._

    v.map { a =>
      val muladder = Module(new MulAddRecFN(expWidth, sigWidth))

      muladder.io.op := 0.U // (a x b) + c
      muladder.io.roundingMode := consts.round_near_even
      muladder.io.detectTininess := consts.tininess_afterRounding

      muladder.io.a := a
      muladder.io.b := s
      muladder.io.c := 0.U

      ShiftRegister(muladder.io.out, latency)
    }
  }
}
