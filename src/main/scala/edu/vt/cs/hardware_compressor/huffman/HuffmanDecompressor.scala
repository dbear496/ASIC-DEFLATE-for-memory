package edu.vt.cs.hardware_compressor.huffman

import chisel3._
import chisel3.util._
import edu.vt.cs.hardware_compressor.util._
import edu.vt.cs.hardware_compressor.util.ArithmeticOps._
import edu.vt.cs.hardware_compressor.util.WidthOps._


// Note: This module uses push input and pull output to facilitate block-style
//  input and output, so one or more universal connectors may be necessary to
//  avoid deadlock and/or circular logic. See documentation for DecoupledStream.
class HuffmanDecompressor(params: Parameters) extends Module {
  val io = IO(new Bundle{
    val in = Vec(params.channelCount, Flipped(DecoupledStream(
      params.decompressorCharsIn, UInt(params.compressedCharBits.W))))
    val out = DecoupledStream(params.decompressorCharsOut,
      UInt(params.characterBits.W))
  })
  
  // wrapped module
  val huffman =
    Module(new huffmanDecompressor.huffmanDecompressor(params.huffman))
  
  // generate a rising edge
  // huffman.io.start := RegNext(true.B, false.B);
  // actually, the decompressor doesn't need a rising edge
  huffman.io.start := true.B
  
  
  //============================================================================
  // DECOMPRESSOR INPUT
  //============================================================================
  
  val inputLast = WireDefault(Bool(), true.B)
  for(i <- 0 until params.channelCount) {
    
    // We do not know how many bits were consumed by the decompressor until one
    // cycle later. So we have to consume the maximum amount and buffer whatever
    // the decompressor does not accept. This complicates the processing of
    // ready-valid.
    
    // 'buffer' holds the characters that were input to the decompressor in the
    // previous cycle. In the current cycle, we can see how many of those were
    // consumed in the previous cycle and re-input the ones that have not yet
    // been consumed.
    
    val buffer = Reg(Vec(params.decompressorCharsIn,
      UInt(params.compressedCharBits.W)))
    val bufferLength = Reg(UInt(params.decompressorCharsIn.valBits.W))
    val bufferBase =
      Reg(UInt(params.huffman.parallelCharactersBitAddressBits.W))
    
    val current = Wire(Vec(params.decompressorCharsIn,
      UInt(params.compressedCharBits.W)))
    
    val currentAddress = huffman.io.currentBit(i)
    val advance = currentAddress - bufferBase
    for(j <- 0 until params.decompressorCharsIn) {
      val bufIdx = advance + j.U
      current(j) := Mux(bufIdx < bufferLength, buffer(bufIdx),
        io.in(i).data(bufIdx - bufferLength))
    }
    io.in(i).ready := params.decompressorCharsIn.U - bufferLength + advance
    
    buffer := current
    bufferLength := (bufferLength - advance +& io.in(i).valid) min
      params.decompressorCharsIn.U
    bufferBase := currentAddress
    
    huffman.io.dataIn(i).bits := current.reduce(_ ## _)
    huffman.io.dataIn(i).valid := (bufferLength - advance +& io.in(i).valid) >=
      params.decompressorCharsIn.U | io.in(i).finished
    
    
    // Because the compressor does not know the number of characters to compress
    // in advance, the variable compression length is not properly encoded. This
    // causes the nested decompressor to hang (and wait for more data) even
    // after all the data is processed. So we cannot report the
    // 'last'/'finished' signal on the output based on the 'finished' signal of
    // the nested decompressor. Instead we must infer when the output is
    // finished based on when the input is finished. This can be done easily
    // because the nested decompressor always outputs the decompressed data in
    // the same cycle as it was input the corresponding compressed data. In any
    // case, this is why we need this 'inputLast' wire; it is used later to
    // infer when the output is finished.
    when(!io.in(i).finished || (bufferLength - advance +& io.in(i).valid) >
        params.decompressorCharsIn.U) {
      inputLast := false.B
    }
  }
  
  
  //============================================================================
  // DECOMPRESSOR OUTPUT
  //============================================================================
  
  huffman.io.dataOut.foreach{output =>
    output.ready := DontCare
  }
  
  val waymodulus = Reg(UInt(params.channelCount.idxBits.W))
  val hold = RegInit(VecInit(Seq.fill(params.channelCount)(false.B)))
  val holdData = Reg(Vec(params.channelCount, UInt(params.characterBits.W)))
  
  var allPrevValid = true.B
  io.out.valid := 0.U
  for(i <- 0 until params.channelCount) {
    val way = (waymodulus +& i.U).div(params.channelCount)._2
    
    when(!hold(way)) {
      io.out.data(i) := huffman.io.dataOut(way).bits
      holdData(way) := huffman.io.dataOut(way).bits
    } otherwise {
      io.out.data(i) := holdData(way)
    }
    
    val ready = i.U <= io.out.ready
    val valid = huffman.io.dataOut(way).valid || hold(way)
    
    huffman.io.dataOut(way).ready := ready && !hold(way)
    
    when(ready && valid) {
      hold(way) := !allPrevValid
    }
    
    if(i != 0)
    when(allPrevValid && i.U < io.out.ready) {
      waymodulus := way
    }
    
    allPrevValid &&= valid
    when(allPrevValid) {
      io.out.valid := (i + 1).U
    }
  }
  
  when(allPrevValid && params.channelCount.U === io.out.ready) {
    waymodulus := waymodulus
  }
  
  io.out.last := inputLast
}

object HuffmanDecompressor extends App {
  val params = Parameters.fromCSV("configFiles/huffman-compat.csv")
  new chisel3.stage.ChiselStage()
    .emitVerilog(new HuffmanDecompressor(params), args)
}
