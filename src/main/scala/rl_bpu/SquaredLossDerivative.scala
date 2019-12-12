
package rl_bpu

import chisel3._
import chisel3.util.ShiftRegister
import hardfloat._

class SquaredLossDerivative(val fp: FloatParams, val in_features: Int, val out_features: Int, val latency: Int = 0)
extends Module {
  // Calculates dL/dW = 2(z - y)(x^T) + 2W

  /*
  L = (z - y)^2 + |W|_2
  L = (Wx + B - y)^2 + |W|_2
  dL/dW = 2(z - y)(x^T) + 2W
  dL/dB = 2(z-y)
   */

  val io = IO(new Bundle {
    val z = Input(Vec(out_features, fp.bits())) // Calculated results
    val y = Input(Vec(out_features, fp.bits())) // Actual results
    val x = Input(Vec(in_features, Bool())) // Inputs
    val w = Input(Vec(out_features, Vec(in_features, fp.bits()))) // Weights

    val learn_rate = Input(fp.bits()) // Learning rate

    val dL_dW = Output(Vec(out_features, Vec(in_features, fp.bits())))
    val dL_dB = Output(Vec(out_features, fp.bits()))
  })

  val float_two = ConstIntToFloat(fp, 2)

  val double_w = MatrixScalarMul(fp, io.w, float_two)

  val z_minus_y = VectorSubtract(fp, io.z, io.y)
  val double_z_minus_y = VectorScalarMul(fp, z_minus_y, float_two)
  val double_z_minus_y_times_x_plus_double_w = VectorVectorMul(fp, double_z_minus_y, io.x, double_w,
    b_is_pn_ones = true)

  val dL = MatrixScalarMul(fp, double_z_minus_y_times_x_plus_double_w, io.learn_rate)

  io.dL_dW := ShiftRegister(VecInit(dL.map(v => VecInit(v))), latency)
  io.dL_dB := ShiftRegister(VecInit(double_z_minus_y), latency)
}
