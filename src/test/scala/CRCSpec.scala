package crc

import chisel3.iotesters.PeekPokeTester

import org.scalatest.{FlatSpec, Matchers}
import scala.util.Random

import java.io.File
import scala.io.Source

class CRCSerialTester(dut: CRCSerialModule) extends PeekPokeTester(dut) {
  step(2)
  poke(dut.io.crcEn, 0)
  step(1)
  poke(dut.io.crcEn, 1)
  poke(dut.io.inBit, 0)
  step(1)
  poke(dut.io.inBit, 0)
  step(1)
  poke(dut.io.inBit, 0)
  step(1)
  poke(dut.io.inBit, 0)
  step(1)
  poke(dut.io.inBit, 0)
  step(1)
  poke(dut.io.inBit, 1)
  step(1)
  poke(dut.io.inBit, 0)
  step(1)
  poke(dut.io.inBit, 1)
  step(50)
}

object CRCSerialTester {
  def apply(poly: String, init: BigInt, order: Int): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"),
      () => new CRCSerialModule(poly, init, order)) { c =>
      new CRCSerialTester(c)
    }
  }
}

class CRCParallelTester(dut: CRCParallelModule, isRadarData: Boolean = false) extends PeekPokeTester(dut) {
  step(2)
  poke(dut.io.crcEn, 0)
  step(1)
  poke(dut.io.crcEn, 1)
  poke(dut.io.inBits, 5)
  step(1)
  poke(dut.io.inBits, 18)
  step(1)
  step(50)
  reset(1)
  poke(dut.io.crcEn, 0)
  if (isRadarData) {
    require(dut.dataWidth == 16, "For radar data dataWidth need to be 16")
    step(2)
    val radarDataSet = Source.fromFile("int16_data_samples0_msb_cp_set1.txt").getLines.toArray.map{ br => br.toInt }
    poke(dut.io.crcEn, 1)
    for (i <- 0 until (radarDataSet.length - 2)) { // do not send crc
      poke(dut.io.inBits, radarDataSet(i))
      step(1)
    }
    step(20)
  }
}

object CRCParallelTester {
  def apply(poly: String, reflectIn: Boolean, init: BigInt, order: Int, dataWidth: Int, isRadarData: Boolean = false): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"),
      () => new CRCParallelModule(poly, reflectIn, init, order, dataWidth)) { c =>
      new CRCParallelTester(c, isRadarData)
    }
  }
}

class CRCSpec extends FlatSpec with Matchers {
  var initCRC = BigInt("FFFFFFFF", 16)
  val poly = "100000100110000010001110110110111"
  val order = 32

  it should s"work serial crc" in {
    CRCSerialTester(poly, init = initCRC, order) should be (true)
  }

  it should s"work parallel crc" in {
    CRCParallelTester(poly = poly, reflectIn = false, init = initCRC, order, dataWidth = 8) should be (true)
  }

  it should s"work parallel crc with radar data" in {
    CRCParallelTester(poly = poly, reflectIn = false, init = initCRC, order, dataWidth = 16, isRadarData = true) should be (true)
  }
}
