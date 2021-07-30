package crc

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._

import chisel3.iotesters.PeekPokeTester

import java.io.File
import scala.io.Source
import org.scalatest.{FlatSpec, Matchers}

class RadarDataCrcCheckerTester(
  dut: AXI4RadarDataCrcChecker with RadarDataCrcCheckerStandaloneBlock,
  crcAddress: AddressSet,
  beatBytes: Int
) extends PeekPokeTester(dut.module) with AXI4MasterModel  {

  override def memAXI: AXI4Bundle = dut.ioMem.get.getWrappedValue
  val radarDataSet = Source.fromFile("int16_data_samples0_msb_cp_set1.txt").getLines.toArray.map{ br => br.toInt }

  poke(dut.in.valid, 1)
  poke(dut.out.ready, 1)

  for (i <- 0 until (radarDataSet.length)) {
    poke(dut.in.valid, 1)
    poke(dut.in.bits.data, radarDataSet(i))
    if (i > radarDataSet.length - 3) {
      poke(dut.module.crcDataActive, 1)
    }
    // simulate valid data at each second clock
    step(1)
    poke(dut.in.valid, 0)
    step(1)
  }
  poke(dut.module.crcDataActive, 0)
  step(100)
}

class RadarDataCrcCheckerTesterSpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty
  val paramsCRC: RadarCRCParams = RadarCRCParams() // use default parameters

  it should "test RadarDataCrcChecker" in {
    val baseAddress = 0x500
    val radarCRCModule = LazyModule(new AXI4RadarDataCrcChecker(paramsCRC, AddressSet(baseAddress + 0x100, 0xFF), _beatBytes = 4) with RadarDataCrcCheckerStandaloneBlock)

    chisel3.iotesters.Driver.execute(Array("-tiwv", "-tbn", "verilator", "-tivsuv"), () => radarCRCModule.module) {
      c => new RadarDataCrcCheckerTester(radarCRCModule, crcAddress = AddressSet(baseAddress + 0x100, 0xFF), beatBytes = 4)
    } should be (true)
  }
}
