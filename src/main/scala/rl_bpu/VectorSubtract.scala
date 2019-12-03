
package rl_bpu

import chisel3._
import chisel3.util.ShiftRegister
import hardfloat._

object VectorSubtract {
  // Returns x - y
  def apply(fp: FloatParams, x: Seq[UInt], y: Seq[UInt], latency: Int = 0): Seq[UInt] = {
    assert(x.size == y.size, "non-matching dimensions")

    import fp.{expWidth, sigWidth}

    val float_one = {
      // This is just the number 1 in floating point format
      val in_to_float = Module(new INToRecFN(1, expWidth, sigWidth))
      in_to_float.io.signedIn := 0.U
      in_to_float.io.in := 1.U
      in_to_float.io.roundingMode := consts.round_near_even
      in_to_float.io.detectTininess := consts.tininess_afterRounding
      in_to_float.io.out
    }

    val diffs = (x zip y).map { case (a, c) =>
      val muladder = Module(new MulAddRecFN(expWidth, sigWidth))

      muladder.io.op := 1.U // (a x b) - c
      muladder.io.roundingMode := consts.round_near_even
      muladder.io.detectTininess := consts.tininess_afterRounding

      muladder.io.a := a
      muladder.io.b := float_one
      muladder.io.c := c

      ShiftRegister(muladder.io.out, latency)
    }

    diffs
  }
}
