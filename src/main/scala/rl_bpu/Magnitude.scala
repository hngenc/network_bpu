
package rl_bpu

import chisel3._
import chisel3.util.ShiftRegister
import hardfloat._

object Magnitude {
  def apply(fp: FloatParams, vector: Seq[UInt], latency: Int): UInt = {
    DotProduct(fp, vector, vector, b_is_pn_ones = false, latency = latency)
  }
}
