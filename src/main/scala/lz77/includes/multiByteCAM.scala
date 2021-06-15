package multiByteCAM

import chisel3._
import chisel3.util._
import lz77Parameters._

class multiByteCAM(params: lz77Parameters) extends Module {
  
  val io = IO(new Bundle {
    // This input allows values to be written into the CAM.
    // val writeData = Input(
    //   Vec(params.compressorMaxCharacters, UInt(params.characterBits.W)))
    // val writeDataLength = Input(UInt(params.compressorMaxCharactersBits.W))
    
    // This input allows for search values to be requested from the CAM.
    val charsIn = Flipped(
      DecoupledStream(params.camMaxCharsIn, UInt(params.characterBits.W)))
    
    // Output a match and the number of literals preceeding the match
    val matchCAMAddress = Output(UInt(params.camAddressBits.W))
    val matchLength = Output(UInt(patternLengthBits.W))
    val literalCount = Output(UInt(params.camMaxCharsInBits.W))
    
    // This output is only used when the multiByteCAM is being used by the lz77Compressor.
    // val camHistory =
    //   if (params.camHistoryAvailable)
    //     Some(Output(Vec(params.camCharacters, UInt(params.characterBits.W))))
    //   else None
  })
  
  // This stores the byte history of the CAM.
  val byteHistory = Mem(params.camCharacters, UInt(params.characterBits.W))
  // This is true iff the camIndex has not yet rolled over
  val camFirstPass = RegInit(true.B)
  // This stores the cam index where the next character will be stored
  val camIndex = RegInit(UInt(params.camAddressBits.W), 0.U)
  
  // CAM indexes eligible for continuation
  val continues =
    RegInit(VecInit(Seq.fill(params.camCharacters, false.B)))
  // the current length of sequences in the continuation
  val continueLength = RegInit(0.U(log2Ceil(params.maxPatternLength).W))
  
  // This handles the write data logic.
  when(camBytes < params.camCharacters.U) {
    when(io.writeData.valid) {
      byteHistory(camBytes) := io.writeData.bits
      camBytes := camBytes + 1.U
    }
  }
  
  // merges byteHistory with searchPattern for easy matching
  val history = Wire(Vec(params.camCharacters + params.camMaxPatternLength,
    UInt(params.characterBits.W)))
  for(i <- 0 until params.camCharacters)
    history(i) := byteHistory(
      if(params.camSizePow2)
        camIndex +% i.U
      else
        Mux(camIndex >= (params.camCharacters - i).U,
          camIndex -% (params.camCharacters - i).U,
          camIndex +% i.U))
  for(i <- 0 until params.camMaxPatternLength)
    history(i + params.camCharacters) := io.charsIn.bits(i)
  
  // find the length of every possible match
  val matchLengths = io.charsIn.bits
    .zipWithIndex
    .map{case (c, i) => Mux(i.U < io.charsIn.valid,
      history.drop(i).take(params.camCharacters).map(_ === c), false.B)}
    .foldRight(Seq.fill(1, params.camCharacters)(0.U)))
      {(equals, counts) =>
        equals.zip(counts(0).map(_ +& 1.U))
          .map{case (e, c) => Mux(e, c, 0.U)}
        +: counts
      }
  
  // find where the match should start in the pattern
  // and rank CAM indexes based on match length
  val matchOptions = Wire(Vec(params.camCharacters,
    UInt(params.camMaxCharsInBits.W)))
  when(continueLength === 0.U) {
    // start a match from scratch
    io.literalCount := PriorityEncoder(matchLengths
      .zipWithIndex
      .map{case (c, i) => (c, params.minCharactersToEncode.U
        min (io.charsIn.valid - i.U))}
      .map{case (c, t) => c
        .map(_ >= t)
        .reduce(_ || _)))
    
    matchOptions := matchLengths(io.literalCount)
  } otherwise {
    // there is a match to continue
    io.literalCount := 0.U
    matchOptions := matchLengths(0)
      .zip(continues)
      .map(a => Mux(a._2, a._1, 0.U))
  }
  
  // compute match length and CAM address
  val (matchLength, matchCAMAddress) = matchOptions.zipWithIndex
    .fold((0.U, 0.U)){case (o, i), (l, a) => (o max l, Mux(o > l, i.U, a))}
  
  // assert outputs
  io.matchLength :=
    Mux(matchLength >= params.minCharactersToEncode.U || continueLength === 0.U,
      matchLength,
      0.U)
  io.charsIn.ready := io.literalCount + io.matchLength
  io.matchCAMAddress := matchCAMAddress
  
  when(io.matchLength === io.charsIn.valid - io.literalCount
      && io.literalCount =/= io.charsIn.valid) {
    continueLength := continueLength + matchLength
    continue := matchOptions.map(_ === matchLength)
  } otherwise {
    continueLength := 0.U
    continue := DontCare
  }
}

object multiByteCAM extends App {
  val settingsGetter = new getLZ77FromCSV()
  val lz77Config = settingsGetter.getLZ77FromCSV("configFiles/lz77.csv")
  chisel3.Driver
    .execute(Array[String](), () => new multiByteCAM(lz77Config))
}
