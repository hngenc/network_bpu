
package rl_bpu

import chisel3._

case class FloatParams(expWidth: Int, sigWidth: Int) {
  def bits(): UInt = UInt((expWidth + sigWidth + 1).W)
}

