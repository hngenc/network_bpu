
package rl_bpu

import chisel3._
import chisel3.util.ShiftRegister
import hardfloat._

object VectorVectorMul {
  def apply(fp: FloatParams, a: Seq[UInt], b: Seq[UInt], bias: Seq[Seq[UInt]],
            b_is_pn_ones: Boolean, latency: Int = 0): Seq[Seq[UInt]] = {
    import fp._

    assert(a.size == bias.size)
    assert(b.size == bias.head.size)

    (a zip bias).map { case (x, bias_) =>
      (b zip bias_).map { case (y, bias__) =>
        val muladder = Module(new MulAddRecFN(expWidth, sigWidth))

        if (b_is_pn_ones) {
          muladder.io.op := Mux(y.toBool(), 0.U, 2.U) // 0.U: (a x b) + c; 2.U: c - (a x b)
        } else {
          muladder.io.op := 0.U // (a x b) + c
        }

        muladder.io.roundingMode := consts.round_near_even
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := x

        if (b_is_pn_ones) {
          muladder.io.b := ConstIntToFloat(fp, 1)
        } else {
          muladder.io.b := y
        }

        muladder.io.c := bias__

        ShiftRegister(muladder.io.out, latency)
      }
    }
  }
}
