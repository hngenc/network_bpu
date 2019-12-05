// See README.md for license details.

package rl_bpu

import chisel3._
import chisel3.util._

class SyncMem2R1W[T <: Data](n: Int, t: T) extends Module {
  val io = IO(new Bundle {
    val raddr = Input(Vec(2, UInt(log2Up(n).W)))
    val rdata = Output(Vec(2, t))
    val ren = Input(Vec(2, Bool()))

    val waddr = Input(UInt(log2Up(n).W))
    val wdata = Input(t)
    val wen = Input(Bool())
  })

  val mem = SyncReadMem(n, t)

  when (io.wen) {
    mem.write(io.waddr, io.wdata)
  }

  for (i <- 0 until 2) {
    io.rdata(i) := RegEnable(mem.read(io.raddr(i), io.ren(i)), RegNext(io.ren(i)))
  }
}
