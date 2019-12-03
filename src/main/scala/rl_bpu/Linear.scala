
package rl_bpu

import chisel3._

class Linear(val fp: FloatParams, val in_features: Int, val out_features: Int,
             val latency: Int = 0)
  extends Module {
  // Inputs:
  //  * features: A vector representing the input features
  //  * weights: A two-dimensional matrix, in row-major form. Each row
  //             represents the weights applying to a particular output neuron
  //  * outputs: A vector representing the outputs features

  val io = IO(new Bundle {
    val features = Input(Vec(in_features, Bool()))
    val weights = Input(Vec(out_features, Vec(in_features, fp.bits())))
    val bias = Input(Vec(out_features, fp.bits()))
    val outputs = Output(Vec(out_features, fp.bits()))
  })

  io.outputs := MatrixVectorMul(fp, io.weights, io.features, io.bias, v_is_pn_ones = true, latency = latency)
}
