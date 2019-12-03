
package rl_bpu

import chisel3._

object MatrixSubtract {
  def apply(fp: FloatParams, x: Seq[Seq[UInt]], y: Seq[Seq[UInt]], latency: Int = 0): Seq[Vec[UInt]] = {
    assert(x.size == y.size)
    assert(x.head.size == y.head.size)

    (x zip y).map { case (a, b) =>
        VecInit(VectorSubtract(fp, a, b, latency))
    }
  }
}
