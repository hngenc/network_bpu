
package rl_bpu

import chisel3._
import chisel3.util._
import hardfloat._

class Network(fp: FloatParams, nFeatures: Int, forward_latency: Int = 0, backward_latency: Int = 0) extends Module {
  import fp.{expWidth, sigWidth}

  val io = IO(new Bundle {
    val inputs = Input(Valid(Vec(nFeatures, Bool())))
    val taken = Output(Valid(Bool()))
    val actual = Input(Valid(new Bundle {
      val branch = Bool()
      val inputs = Vec(nFeatures, Bool())
    }))
  })

  val weights = Reg(Vec(2, Vec(nFeatures, fp.bits())))
  val bias = Reg(Vec(2, fp.bits()))

  val fc = Module(new Linear(fp, nFeatures, 2, latency = forward_latency))
  FlattenInst(fc)

  fc.io.features := io.inputs.bits
  fc.io.weights := weights
  fc.io.bias := bias

  val taken_prob = fc.io.outputs(0)
  val not_taken_prob = fc.io.outputs(1)

  val comparer = Module(new CompareRecFN(expWidth, sigWidth))
  comparer.io.a := taken_prob
  comparer.io.b := not_taken_prob
  comparer.io.signaling := false.B

  io.taken.valid := ShiftRegister(io.inputs.valid, forward_latency)
  io.taken.bits := comparer.io.gt

  val prediction_buffer = Module(new Queue(new Bundle {
    val z = Vec(2, fp.bits())
    // val x = Vec(nFeatures, fp.bits())
  }, 8))
  prediction_buffer.io.enq.valid := io.taken.valid
  prediction_buffer.io.enq.bits.z := fc.io.outputs
  prediction_buffer.io.deq.ready := io.actual.valid

  val x = io.actual.bits.inputs
  val y = Mux(io.actual.bits.branch, VecInit(ConstIntToFloat(fp, 1), 0.U), VecInit(0.U, ConstIntToFloat(fp, 1)))

  val deriver = Module(new SquaredLossDerivative(fp, nFeatures, 2, latency = backward_latency))
  deriver.io.z := prediction_buffer.io.deq.bits.z
  deriver.io.y := y
  deriver.io.x := x
  deriver.io.w := weights // TODO do we need to buffer the weights as well?
  deriver.io.learn_rate := ConstFloatToFloat(fp, 0.0078125f)

  when (ShiftRegister(io.actual.fire(), backward_latency)) {
    weights := MatrixSubtract(fp, weights, deriver.io.dL_dW)
    bias := VectorSubtract(fp, bias, deriver.io.dL_dB)
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
  val fp = FloatParams(expWidth = 8, sigWidth = 7)

  chisel3.Driver.execute(Array[String](), () => new Network(fp, nFeatures, forward_latency, backward_latency) {
    override def desiredName: String = s"Network_${forward_latency}_$backward_latency"
  })
}
