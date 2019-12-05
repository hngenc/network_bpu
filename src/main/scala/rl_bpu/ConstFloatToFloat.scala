// See README.md for license details.


package rl_bpu

import chisel3._
import hardfloat._

object ConstFloatToFloat {
  def apply(fp: FloatParams, const: Float): UInt = {
    /*
    val const_as_int = java.lang.Float.floatToIntBits(const)

    val sign = const_as_int >> 31
    val exp = (const_as_int >> 23) & ((1 << 8) - 1)
    val mantissa = const_as_int & ((1 << 23) - 1)
    */

    val const_as_uint = java.lang.Float.floatToIntBits(const).U

    val ieee754f_exp_bits = 8
    val ieee754f_mantissa_bits = 23

    val const_as_rec_fn = recFNFromFN(ieee754f_exp_bits + 1, ieee754f_mantissa_bits, const_as_uint)

    val rec_fn_reshaper = Module(
      new RecFNToRecFN(ieee754f_exp_bits + 1, ieee754f_mantissa_bits, fp.expWidth, fp.sigWidth)
    )

    rec_fn_reshaper.io.in := const_as_rec_fn
    rec_fn_reshaper.io.roundingMode := consts.round_near_even
    rec_fn_reshaper.io.detectTininess := consts.tininess_afterRounding

    rec_fn_reshaper.io.out
  }
}
