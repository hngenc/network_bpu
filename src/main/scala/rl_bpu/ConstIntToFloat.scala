
package rl_bpu

import chisel3._
import chisel3.util._
import hardfloat._

object ConstIntToFloat {
  def apply(fp: FloatParams, const: Int): UInt = {
    import fp.{expWidth, sigWidth}

    val isSigned = if (const < 0) 1 else 0
    val intWidth = log2Up(const + 1) + isSigned

    // This is just the number 1 in floating point format
    val in_to_float = Module(new INToRecFN(intWidth, expWidth, sigWidth))
    in_to_float.io.signedIn := isSigned.U
    in_to_float.io.in := const.U
    in_to_float.io.roundingMode := consts.round_near_even
    in_to_float.io.detectTininess := consts.tininess_afterRounding
    in_to_float.io.out
  }
}
