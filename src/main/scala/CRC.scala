package crc

import chisel3._
import chisel3.util._

import chisel3.stage.{ChiselGeneratorAnnotation}

// Should generate simple logic for serial CRC
object CRCSerial {
  def apply(poly: String, crcOld: UInt, inBit: Bool): UInt = {
    val order = poly.toCharArray.length
    val polyBits = CRC.convertBinStringToIntArray(poly.reverse.tail)
    val crcNew = VecInit(crcOld.asBools)

    // can be implemented in one line think about it.
    crcNew(0) := inBit ^ crcOld(order-2)//crcOld(order-1)
    for (i <- 1 until (order-1)) {
      if (polyBits(i-1) == 1) {
        crcNew(i) :=  crcOld(order-2) ^ inBit ^ crcOld(i-1)
      }
      else {
        crcNew(i) := crcOld(i-1)
      }
    }
    crcNew.asUInt
  }
}
object CRCParallel {
  def apply(poly: String, crcOld: UInt, inData: UInt, dataWidth: Int): UInt = {
    if (dataWidth != 0) {
      CRCSerial(poly, apply(poly, crcOld, inData, dataWidth-1), inData(dataWidth-1))
    }
    else {
      crcOld
    }
  }
}

class CRCIO(len: Int) extends Bundle {
  val crcEn  = Input(Bool())
  val inBit  = Input(Bool())
  val outCrc = Output(UInt(len.W))
}

// simple test for CRCSerial
class CRCSerialModule(val poly: String, val init: BigInt, val order: Int) extends Module {
  val io = IO(new CRCIO(order))
  val crc = RegInit(init.U(order.W))

  when (io.crcEn) {
    crc := CRCSerial(poly = poly, crcOld = crc, io.inBit)
  }
  io.outCrc := crc
}

class CRCParallelIO(len: Int, dataWidth: Int) extends Bundle {
  val crcEn = Input(Bool())              // means that inBits are
  val inBits  = Input(UInt(dataWidth.W))
  val outCrc = Output(UInt(len.W))
}


class CRCParallelModule(val poly: String, val reflectIn: Boolean, val init: BigInt, val order: Int, val dataWidth: Int) extends Module {
  val io = IO(new CRCParallelIO(order, dataWidth))
  val crc = RegInit(init.U(order.W))

  // think how to make this more logical
  val inToCRC = if (reflectIn) io.inBits else Reverse(io.inBits)

  when (io.crcEn) {
    crc := CRCParallel(poly, crc, inToCRC, dataWidth)
  }
  io.outCrc := crc
}

object CRCSerialApp extends App
{
  var initCRC = BigInt("FFFFFFFF", 16)

  val poly = "100000100110000010001110110110111"
  (new chisel3.stage.ChiselStage).execute(Array("-X", "verilog", "--target-dir", "generated-rtl"),
  Seq(ChiselGeneratorAnnotation(() => new CRCSerialModule(poly = poly, init = initCRC, 32))))
}

object CRCParallelApp extends App
{
  var initCRC = BigInt("FFFFFFFF", 16)

  val poly = "100000100110000010001110110110111"
  (new chisel3.stage.ChiselStage).execute(Array("-X", "verilog", "--target-dir", "generated-rtl"),
  Seq(ChiselGeneratorAnnotation(() => new CRCParallelModule(poly, reflectIn = false, init = initCRC, 32, 8))))
}
