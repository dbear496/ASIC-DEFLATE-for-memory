package multiByteCAM

import chisel3._
import chisel3.util._
import lz77Parameters._
import lz77.util._

class multiByteCAM(params: lz77Parameters) extends Module {
  
  val io = IO(new Bundle {
    val charsIn = Flipped(DecoupledStream(
      params.camMaxCharsIn, UInt(params.characterBits.W)))
    val maxLiteralCount = Input(UInt(params.camMaxCharsInBits.W))
    
    // Output a match and the number of literals preceeding the match
    val matchCAMAddress = Output(UInt(params.camAddressBits.W))
    val matchLength = Output(UInt(params.patternLengthBits.W))
    val literalCount = Output(UInt(params.camMaxCharsInBits.W))
    
    val finished = Output(Bool())
  })
  
  
  // This stores the byte history of the CAM.
  val byteHistory = Mem(params.camCharacters, UInt(params.characterBits.W))
  // This is true iff the camIndex has not yet rolled over
  val camFirstPass = RegInit(true.B)
  // This stores the cam index where the next character will be stored
  val camIndex = RegInit(UInt(params.camAddressBits.W), 0.U)
  
  
  // CAM indexes eligible for continuation
  val continues =
    RegInit(VecInit(Seq.fill(params.camCharacters)(false.B)))
  // the current length of sequences in the continuation
  val continueLength = RegInit(0.U(log2Ceil(params.maxPatternLength).W))
  
  
  // write data to history
  for(index <- 0 until io.charsIn.bits.length)
    when(index.U < io.charsIn.ready) {
      byteHistory(
        if(params.camSizePow2)
          (camIndex + index.U)(params.camAddressBits - 1, 0)
        else
          (camIndex +& index.U) % params.camCharacters.U
      ) := io.charsIn.bits(index)
    }
  if(params.camSizePow2) camIndex := camIndex + io.charsIn.ready
  else camIndex := (camIndex +& io.charsIn.ready) % params.camCharacters.U
  camFirstPass := camFirstPass &&
    (io.charsIn.ready < params.camCharacters.U - camIndex)
  
  
  // merge byteHistory and searchPattern for easy matching
  val history =
    (0 until params.camCharacters)
      .map{i => byteHistory(
        if(params.camSizePow2)
          i.U +% camIndex
        else
          Mux(camIndex < (params.camCharacters - i).U,
            camIndex +% i.U,
            camIndex -% (params.camCharacters - i).U)
      )} ++
      io.charsIn.bits
  
  
  // find the length of every possible match
  val matchLengths = io.charsIn.bits
    .zipWithIndex
    .map{case (c, i) =>
      history
        .drop(i)
        .take(params.camCharacters)
        .map(_ === c && i.U < io.charsIn.valid)}
    .foldRight(
      Seq.fill(1, params.camCharacters)(0.U(params.camMaxCharsInBits.W)))
      {(equals, counts) =>
        equals
          .zip(counts(0).map(_ + 1.U(params.camMaxCharsInBits.W)))
          .map{case (e, c) =>
            Mux(e, c, 0.U(params.camMaxCharsInBits.W))} +:
        counts
      }
  
  
  // find where the match should start in the pattern
  // and rank CAM indexes based on match length
  val matchRow =
    Wire(Vec(params.camCharacters, UInt(params.camMaxCharsInBits.W)))
  val literalCount = Wire(UInt(params.camMaxCharsInBits.W))
  when(continueLength === 0.U) {
    // start a match from scratch
    
    class Row extends Bundle {
      val row = Vec(params.camCharacters, UInt(params.characterBits.W))
      val lit = UInt(params.camMaxCharsInBits.W)
    }
    
    val row = PriorityMux(
      matchLengths.zipWithIndex.map{case (lens, lit) =>
        val curRow = Wire(new Row)
        curRow.row := VecInit(lens)
        curRow.lit := lit.U
        ( lens
            .map(len =>
              len >= params.minCharactersToEncode.U ||
              len === io.charsIn.valid - lit.U)
            .reduce(_ || _),
          curRow)})
    
    literalCount := row.lit
    matchRow := row.row
    
  } otherwise {
    // there is a match to continue
    matchRow := matchLengths(0)
      .zip(continues)
      .map(a => Mux(a._2, a._1, 0.U))
    
    literalCount := 0.U
  }
  
  val (nolimitMatchLength, matchCAMAddress) = matchRow
    .zipWithIndex
    .map{case (len, add) => (len, add.U)}
    .reduce[(UInt, UInt)]{case ((len1, add1), (len2, add2)) =>
      val is1 = len1 >= len2
      ( Mux(is1, len1, len2),
        Mux(is1, add1, add2))
    }
  val matchLength =
    nolimitMatchLength min (params.maxPatternLength.U - continueLength)
  
  io.finished := false.B
  io.matchLength := 0.U
  io.matchCAMAddress := DontCare
  io.literalCount := literalCount
  continueLength := 0.U
  continues := DontCare
  
  when(continueLength =/= 0.U || matchLength >= params.minCharactersToEncode.U){
    when(matchLength + literalCount =/= io.charsIn.valid ||
        io.charsIn.finished) {
      io.matchLength := continueLength + matchLength
      io.matchCAMAddress := Mux(matchLength === 0.U,
        PriorityEncoder(continues),
        matchCAMAddress)
    }.elsewhen(literalCount <= io.maxLiteralCount) {
      continueLength := continueLength + matchLength
      continues := matchRow.map(_ === io.charsIn.valid - literalCount)
    }
  }.elsewhen(io.charsIn.finished) {
    io.literalCount := literalCount + matchLength
  }
  
  when(continueLength =/= 0.U) {
    io.charsIn.ready := matchLength
  }.elsewhen(literalCount > io.maxLiteralCount) {
    io.charsIn.ready := io.maxLiteralCount
  }.elsewhen(matchLength >= params.minCharactersToEncode.U) {
    io.charsIn.ready := literalCount + matchLength
  } otherwise {
    io.charsIn.ready := io.literalCount
  }
  
  // compute finished
  io.finished := io.charsIn.finished && io.charsIn.ready === io.charsIn.valid
}

object multiByteCAM extends App {
  val settingsGetter = new getLZ77FromCSV()
  val lz77Config = settingsGetter.getLZ77FromCSV("configFiles/lz77.csv")
  chisel3.Driver
    .execute(Array[String](), () => new multiByteCAM(lz77Config))
}
