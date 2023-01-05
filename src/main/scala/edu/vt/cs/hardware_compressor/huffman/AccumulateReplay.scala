package edu.vt.cs.hardware_compressor.huffman

import chisel3._
import chisel3.util._
import edu.vt.cs.hardware_compressor.util._
import edu.vt.cs.hardware_compressor.util.WidthOps._


class AccumulateReplay(params: Parameters) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(RestartableDecoupledStream(params.compressorCharsIn, UInt(params.characterBits.W)))
    val out = RestartableDecoupledStream(params.encoderParallelism, UInt(params.characterBits.W))
  })
  
  val mem = Mem(params.accRepBufferSize, UInt(params.characterBits.W))
  val head = RegInit(UInt(params.accRepBufferSize.idxBits.W), 0.U)
  val tail = RegInit(UInt(params.accRepBufferSize.idxBits.W), 0.U)
  
  val isMarked = RegInit(Bool(), false.B)
  val mark = RegInit(UInt(params.accRepBufferSize.idxBits.W), 0.U)
  val cap = Mux(isMarked, mark, tail)
  
  val inbuf = Reg(Vec(params.compressorCharsIn, UInt(params.characterBits.W)))
  val inbufLen = RegInit(UInt(params.compressorCharsIn.valBits.W), 0.U)
  val outbuf = Reg(Vec(params.encoderParallelism, UInt(params.characterBits.W)))
  val outbufLen = RegInit(UInt(params.encoderParallelism.valBits.W), 0.U)
  
  // load from memory
  val unconsumed = Mux(io.out.ready < outbufLen, outbufLen - io.out.ready, 0.U)
  val memread = WireDefault(Vec(params.encoderParallelism, UInt(params.characterBits.W)), DontCare)
  val outbuf_tmp0 = Wire(Vec(params.encoderParallelism * 2, UInt(params.characterBits.W)))
  for(i <- 0 until params.encoderParallelism) {
    // get next bytes from memory using minimal read ports
    memread((i.U - head)(params.encoderParallelism.idxBits - 1,0)) := mem(((head + (params.encoderParallelism - 1).U - i.U) & (params.accRepBufferSize - params.encoderParallelism).U) + i.U)
    // append next bytes from memory to existing data from the buffer
    outbuf_tmp0(i) := Mux(i.U < outbufLen, outbuf(i), memread(i.U - outbufLen))
    outbuf_tmp0(i + params.encoderParallelism) := memread((i + params.encoderParallelism).U - outbufLen)
    // compute the value of outbuf(i) for each potential io.out.ready
    val outbuf_tmp1 = VecInit(outbuf_tmp0
      .drop(i).take(params.encoderParallelism)
      .zipWithIndex
      .map(d => Mux(d._2.U < outbufLen, d._1, memread(i)))
    )
    outbuf(i) := outbuf_tmp1(io.out.ready)
  }
  // when(io.out.ready > outbufLen) {
  //   outbuf := memread
  // }
  head := head + ((cap - head)(params.accRepBufferSize.idxBits - 1,0) min (params.encoderParallelism.U - unconsumed))
  
  // store to memory
  for(i <- 0 until params.counterCharsIn) {
    val a = ((tail + (params.counterCharsIn - 1).U - i.U) & (params.accRepBufferSize - params.counterCharsIn).U) + i.U
    when((tail >= head) ^ (a >= tail) ^ (a >= head)) {
      mem(a) := inbuf((i.U - tail)(params.counterCharsIn.idxBits - 1,0))
    }
  }
  tail := tail + inbufLen
  
  io.out.data := outbuf
  io.out.valid := outbufLen
  io.out.last := isMarked && head === mark
  outbufLen := (unconsumed +& (cap - head)(params.accRepBufferSize.idxBits - 1,0)) min params.encoderParallelism.U
  when(io.out.restart) {
    isMarked := false.B
    head := mark // for good measure
    outbufLen := 0.U // for good measure
  }
  
  io.in.ready := (head - tail - inbufLen - 1.U)(params.accRepBufferSize.idxBits - 1,0) min params.counterCharsIn.U
  io.in.restart := false.B
  inbuf := io.in.data
  inbufLen := io.in.valid min io.in.ready
  when(!isMarked && io.in.last && io.in.valid === 0.U) {
    io.in.restart := true.B
    isMarked := true.B
    mark := tail + inbufLen
  }
}
