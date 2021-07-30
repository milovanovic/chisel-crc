package crc

import chisel3._
import chisel3.util._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._

case class RadarCRCParams(
  // CRC module specific parameters
  poly: String = "100000100110000010001110110110111",
  reflectIn: Boolean = false,
  init: BigInt = BigInt("FFFFFFFF", 16),
  order: Int = 32,
  dataWidth: Int = 16,
  // Radar specific crc parameters
  CRCErrorLine: Boolean = true,
  reflectOut: Boolean = true,
  crcMask: BigInt = BigInt("FFFFFFFF", 16),
  crcXor: BigInt = BigInt("FFFFFFFF", 16)
) {
  // add some requirements
}

trait RadarDataCrcCheckerStandaloneBlock extends AXI4RadarDataCrcChecker {
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
      BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
      ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }

}

abstract class RadarDataCrcChecker [D, U, E, O, B <: Data] (params: RadarCRCParams, beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {

  val streamNode = AXI4StreamIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    // input is
    val (in, _)  = streamNode.in(0)
    val (out, _) = streamNode.out(0)

    val crcDataActive = IO(Input(Bool()))
    val crc = Module(new CRCParallelModule(poly = params.poly,
                                           reflectIn = params.reflectIn,
                                           init = params.init,
                                           order = params.order,
                                           dataWidth = params.dataWidth))

    // used for debug purposes
    val crcError = if (params.CRCErrorLine) Some(IO(Output(Bool()))) else None

    // control registers
    val crcEnable = RegInit(true.B) // by default CRC is enabled
    val passthroughCrc = RegInit(false.B)
    // status registers
    val crcErrorStatus = RegInit(false.B)
    val receivedCRCData = WireInit(0.U(params.order.W))
    val crcOnLineValid = in.valid && in.ready && crcDataActive
    val LSBcollected = RegInit(false.B)
    val frameFinishedFlag = RegInit(false.B)
    val LSBCrcData = RegEnable(in.bits.data, 0.U, crcOnLineValid)
    // this is to be compatible with defined function inside ti radar literature
    val crcToCheck = (Reverse(crc.io.outCrc) ^ params.crcXor.U(params.order.W)) & params.crcMask.U(params.order.W)

    when (crcOnLineValid) {
      LSBcollected := true.B
    }

    when (crcOnLineValid && LSBcollected) {
      LSBcollected := false.B
      receivedCRCData := Cat(LSBCrcData, in.bits.data)
      if (params.CRCErrorLine) {
        crcError.get := (receivedCRCData =/= crcToCheck)
      }
      frameFinishedFlag := true.B
      crcErrorStatus := (receivedCRCData =/= crcToCheck)
    }
    .otherwise {
      receivedCRCData := 0.U
      if (params.CRCErrorLine) {
        crcError.get := false.B
      }
    }

    // when new frame is detected then status register should be reset
    when (in.valid && in.ready && frameFinishedFlag) {
      frameFinishedFlag := false.B
    }
    when (frameFinishedFlag === false.B) {
      crcErrorStatus := false.B
    }
   // define fields
    val fields = Seq(
      RegField(1, crcEnable, RegFieldDesc(name = "crcEnable", desc = "Enable crc checker")),
      RegField(1, passthroughCrc, RegFieldDesc(name = "passtroughCrc", desc = "If this register is active CRC is not removed from stream")),
      RegField.r(1, crcErrorStatus, RegFieldDesc(name = "crcErrorStatus", desc = "Status register used for crc error detection"))
    )

    // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
    regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)

    crc.io.crcEn  := (in.valid && in.ready) && crcEnable && ~crcDataActive
    crc.io.inBits := in.bits.data
    in.ready := out.ready

    // Connect output
    when (passthroughCrc) {
      out.valid     := in.valid
    }
    .otherwise {
      out.valid     := in.valid && ~crcDataActive
    }
    out.bits.data := in.bits.data // data are not valid when crc is active
  }
}

class AXI4RadarDataCrcChecker(params: RadarCRCParams, address: AddressSet, _beatBytes: Int = 4)(implicit p: Parameters) extends RadarDataCrcChecker[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, _beatBytes) with AXI4DspBlock with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = _beatBytes))
}

object RadarDataCrcCheckerApp extends App
{
  val paramsCRC: RadarCRCParams = RadarCRCParams() // use default parameters

  val baseAddress = 0x500 // just to check if verilog code is succesfully generated or not
  implicit val p: Parameters = Parameters.empty
  val radarCRCModule = LazyModule(new AXI4RadarDataCrcChecker(paramsCRC, AddressSet(baseAddress + 0x100, 0xFF), _beatBytes = 4) with RadarDataCrcCheckerStandaloneBlock)
  chisel3.Driver.execute(args, ()=> radarCRCModule.module)
}
