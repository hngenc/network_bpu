
package rl_bpu

import chisel3._
import chisel3.util.ShiftRegister
import hardfloat._

class SquaredLoss(val fp: FloatParams, val latency: Int = 0) extends Module {
  import fp.{expWidth, sigWidth}

  val io = IO(new Bundle {
    val y = Input(fp.bits())
    val z = Input(fp.bits())
    val regularization_cost = Input(fp.bits())
    val loss = Output(fp.bits())
  })

  val diff = {
    val muladder = Module(new MulAddRecFN(expWidth, sigWidth))

    muladder.io.op := 1.U // (a x b) - c
    muladder.io.roundingMode := consts.round_near_even
    muladder.io.detectTininess := consts.tininess_afterRounding

    muladder.io.a := io.y
    muladder.io.b := ConstIntToFloat(fp, 1)
    muladder.io.c := io.z

    muladder.io.out
  }

  val loss = {
    val muladder = Module(new MulAddRecFN(expWidth, sigWidth))

    muladder.io.op := 0.U // (a x b) + c
    muladder.io.roundingMode := consts.round_near_even
    muladder.io.detectTininess := consts.tininess_afterRounding

    muladder.io.a := diff
    muladder.io.b := diff
    muladder.io.c := io.regularization_cost

    muladder.io.out
  }

  io.loss := ShiftRegister(loss, latency)
}
