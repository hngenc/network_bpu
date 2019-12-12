// See README.md for license details.

package rl_bpu

import chisel3._
import chisel3.util._
import hardfloat._

class Network(fp: FloatParams, nFeatures: Int, nWeightRows: Int,
              forward_latency: Int = 0, backward_latency: Int = 0) extends Module {
  import fp.{expWidth, sigWidth}

  val io = IO(new Bundle {
    val req = Flipped(Valid(new Bundle {
      val inputs = Vec(nFeatures, Bool())
      val weights_row = UInt(log2Up(nWeightRows).W)
    }))

    val taken = Valid(Bool())

    val actual = Flipped(Valid(new Bundle {
      val branch = Bool()
      val inputs = Vec(nFeatures, Bool())
      val weights_row = UInt(log2Up(nWeightRows).W)
    }))
  })

  val weights = Module(new SyncMem2R1W(nWeightRows, Vec(2, Vec(nFeatures, fp.bits()))))
  val biases = Module(new SyncMem2R1W(nWeightRows, Vec(2, fp.bits())))

  weights.io.wen := false.B
  weights.io.waddr := DontCare
  weights.io.wdata := DontCare

  biases.io.wen := false.B
  biases.io.waddr := DontCare
  biases.io.wdata := DontCare

  // Predict
  weights.io.ren(0) := io.req.valid
  weights.io.raddr(0) := io.req.bits.weights_row

  biases.io.ren(0) := io.req.valid
  biases.io.raddr(0) := io.req.bits.weights_row

  val fc = Module(new Linear(fp, nFeatures, 2, latency = forward_latency))
  FlattenInst(fc)

  fc.io.features := ShiftRegister(io.req.bits.inputs, 2)
  fc.io.weights := weights.io.rdata(0)
  fc.io.bias := biases.io.rdata(0)

  val taken_prob = fc.io.outputs(0)
  val not_taken_prob = fc.io.outputs(1)

  val comparer = Module(new CompareRecFN(expWidth, sigWidth))
  comparer.io.a := taken_prob
  comparer.io.b := not_taken_prob
  comparer.io.signaling := false.B

  io.taken.valid := ShiftRegister(io.req.valid, forward_latency + 2)
  io.taken.bits := comparer.io.gt

  // Update
  weights.io.ren(1) := io.actual.valid
  weights.io.raddr(1) := io.actual.bits.weights_row

  biases.io.ren(1) := io.actual.valid
  biases.io.raddr(1) := io.actual.bits.weights_row

  val prediction_buffer = Module(new Queue(new Bundle {
    val z = Vec(2, fp.bits())
  }, 8))
  prediction_buffer.io.enq.valid := io.taken.valid
  prediction_buffer.io.enq.bits.z := fc.io.outputs
  prediction_buffer.io.deq.ready := io.actual.valid

  val x = ShiftRegister(io.actual.bits.inputs, 2)
  val y = ShiftRegister(
    Mux(io.actual.bits.branch, VecInit(ConstIntToFloat(fp, 1), 0.U), VecInit(0.U, ConstIntToFloat(fp, 1))),
    2
  )
  val z = ShiftRegister(prediction_buffer.io.deq.bits.z, 2)

  val deriver = Module(new SquaredLossDerivative(fp, nFeatures, 2, latency = backward_latency))
  FlattenInst(deriver)

  deriver.io.z := z
  deriver.io.y := y
  deriver.io.x := x
  deriver.io.w := weights.io.rdata(1)
  deriver.io.learn_rate := ConstFloatToFloat(fp, 0.0078125f)

  when (ShiftRegister(io.actual.fire(), backward_latency + 2)) {
    val current_weights = ShiftRegister(weights.io.rdata(1), backward_latency)
    val current_bias = ShiftRegister(biases.io.rdata(1), backward_latency)

    weights.io.wen := true.B
    weights.io.waddr := ShiftRegister(io.actual.bits.weights_row, backward_latency + 2)
    weights.io.wdata := MatrixSubtract(fp, current_weights, deriver.io.dL_dW)

    biases.io.wen := true.B
    biases.io.waddr := ShiftRegister(io.actual.bits.weights_row, backward_latency + 2)
    biases.io.wdata := VectorSubtract(fp, current_bias, deriver.io.dL_dB)
  }
}

object BuildNetwork extends App {
  val usage = "runMain rl_bpu.BuildNetwork [-f forward_latency] [-b backward_latency] [-h] [--help]"

  type OptionMap = Map[Symbol, Int]
  def nextOption(args: List[String]): OptionMap = {
    args match {
      case "-h" :: _ | "--help" :: _ => {
        println(usage)
        System.exit(0)
        throw new Exception("Unreachable")
      }
      case "-f" :: latency :: tail => {
        Map('forward_latency -> latency.toInt) ++ nextOption(tail)
      }
      case "-b" :: latency :: tail => {
        Map('backward_latency -> latency.toInt) ++ nextOption(tail)
      }
      case Nil => Map()
      case _ => throw new Exception("Unknown options")
    }
  }

  val optionMap = nextOption(args.toList)

  val forward_latency = optionMap.getOrElse('forward_latency, 0)
  val backward_latency = optionMap.getOrElse('backward_latency, 0)
  val nFeatures = 40
  val nWeightRows = 64
  val fp = FloatParams(expWidth = 8, sigWidth = 7)

  chisel3.Driver.execute(Array[String](),
    () => new Network(fp, nFeatures, nWeightRows, forward_latency, backward_latency) {
      override def desiredName: String = s"Network_${forward_latency}_$backward_latency"
    }
  )
}
