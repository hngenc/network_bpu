
package rl_bpu

import chisel3._
import chisel3.util._
import hardfloat._

object DotProduct {
  // Compute a.dot(b) + bias
  def apply(fp: FloatParams, a: Seq[UInt], b: Seq[UInt], bias: UInt = 0.U, b_is_pn_ones: Boolean,
            latency: Int = 0): UInt = {
    assert(a.size == b.size)

    val pairs = a zip b
    val (acc, addport) = constructMulAddTree(fp, pairs, b_is_pn_ones)
    addport := bias
    ShiftRegister(acc, latency)
  }

  def constructMulAddTree(fp: FloatParams, pairs: Seq[Tuple2[UInt,UInt]], b_is_pn_ones: Boolean): Tuple2[UInt, UInt] = {
    // This function takes in tuple-pairs of features and weights.
    // It returns a port from which the result may be read, and one port from which something may be added to the result

    import fp._

    if (pairs.size == 1) {
      val (a, b) = pairs.head

      val muladder = Module(new MulAddRecFN(expWidth, sigWidth))

      if (b_is_pn_ones) {
        muladder.io.op := Mux(b.toBool(), 0.U, 2.U) // 0.U: (a x b) + c; 2.U: c - (a x b)
      } else {
        muladder.io.op := 0.U // (a x b) + c
      }

      muladder.io.roundingMode := consts.round_near_even
      muladder.io.detectTininess := consts.tininess_afterRounding

      muladder.io.a := a

      if (b_is_pn_ones) {
        muladder.io.b := ConstIntToFloat(fp, 1)
      } else {
        muladder.io.b := b
      }

      (muladder.io.out, muladder.io.c)
    } else {
      val mid = pairs.size / 2
      val (lo, hi) = pairs.splitAt(mid)

      val (external_acc, internal_add) = constructMulAddTree(fp, lo, b_is_pn_ones)
      val (internal_acc, external_add) = constructMulAddTree(fp, hi, b_is_pn_ones)

      internal_add := internal_acc

      (external_acc, external_add)
    }
  }
}
