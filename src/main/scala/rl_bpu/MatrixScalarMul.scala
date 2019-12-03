
package rl_bpu

import chisel3._
import chisel3.util.ShiftRegister
import hardfloat._

object MatrixScalarMul {
  def apply(fp: FloatParams, m: Seq[Seq[UInt]], s: UInt, latency: Int = 0): Seq[Seq[UInt]] = {
    m.map { v =>
      VectorScalarMul(fp, v, s, latency)
    }
  }
}
