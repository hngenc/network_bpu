
package rl_bpu

import chisel3._
import chisel3.util._
import hardfloat._

object MatrixVectorMul {
  def apply(fp: FloatParams, mat: Seq[Seq[UInt]], v: Seq[UInt], bias: Seq[UInt],
            v_is_pn_ones: Boolean, latency: Int = 0): Seq[UInt] = {
    // This calculates weights * features

    assert(mat.size == bias.size)
    assert(mat.head.size == v.size)

    (mat, bias).zipped.map { case (row, b) =>
        DotProduct(fp, row, v, b, v_is_pn_ones, latency)
    }
  }
}
