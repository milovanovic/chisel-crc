package crc

import chisel3._
import chisel3.util._
import chisel3.experimental.{IO}
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

trait CrcCheckerPins extends CrcChecker {

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }

  // pins
  def makeCustomIO(): CrcCheckerIO = {
    val io2: CrcCheckerIO = IO(io.cloneType)
    io2.suggestName("io")
    io2 <> io
    io2
  }
  val ioBlock = InModuleBody { makeCustomIO() }

}

class CrcCheckerIO extends Bundle {
  val i_crcDataActive  = Input(Bool())
  val i_crcEnable      = Input(Bool())
  val i_passthroughCrc = Input(Bool())
  val o_crcErrorStatus = Output(Bool())
}

class CrcChecker(params: RadarCRCParams) extends LazyModule()(Parameters.empty) {

  val streamNode = AXI4StreamIdentityNode()
  // IO
  lazy val io = Wire(new CrcCheckerIO)

  lazy val module = new LazyModuleImp(this) {
    // input is
    val (in, _)  = streamNode.in(0)
    val (out, _) = streamNode.out(0)

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
    crcEnable      := io.i_crcEnable
    passthroughCrc := io.i_passthroughCrc
    io.o_crcErrorStatus := crcErrorStatus

    val receivedCRCData = WireInit(0.U(params.order.W))
    val crcOnLineValid = in.valid && in.ready && io.i_crcDataActive
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

    crc.io.crcEn  := (in.valid && in.ready) && crcEnable && ~io.i_crcDataActive
    crc.io.inBits := (in.bits.data>>(16-params.dataWidth)).asTypeOf(crc.io.inBits)
    in.ready := out.ready

    // Connect output
    when (passthroughCrc) {
      out.valid := in.valid
    }
    .otherwise {
      out.valid := in.valid && ~io.i_crcDataActive
    }
    out.bits.data := in.bits.data // data are not valid when crc is active
  }
}

object CrcCheckerApp extends App
{
  val params = RadarCRCParams(
    // CRC module specific parameters
    poly      = "100000100110000010001110110110111",
    reflectIn = false,
    init      = BigInt("FFFFFFFF", 16),
    order     = 32,
    dataWidth = 16,
    // Radar specific crc parameters
    CRCErrorLine = true,
    reflectOut   = true,
    crcMask      = BigInt("FFFFFFFF", 16),
    crcXor       = BigInt("FFFFFFFF", 16)
  )
  implicit val p: Parameters = Parameters.empty
  val lazyDut = LazyModule(new CrcChecker(params) with CrcCheckerPins)
  (new ChiselStage).execute(Array("--target-dir", "verilog/CrcChecker"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}
