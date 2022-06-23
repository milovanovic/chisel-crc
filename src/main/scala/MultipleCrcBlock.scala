package crc

import chisel3._
import chisel3.util.Cat
import chisel3.experimental.IO
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._

case class MiltipleCrcBlockParams(
  crcParams16 : Option[RadarCRCParams],
  crcParams12 : Option[RadarCRCParams],
  crcParams14 : Option[RadarCRCParams],
)
// MiltipleCrcBlock Bundle
class MiltipleCrcBlockIO(channels: Int) extends Bundle {
    val word_size = if (channels > 1) Some(Input(UInt(2.W))) else None
    val crc_en    = if (channels >= 1) Some(Input(UInt(1.W))) else None

    override def cloneType: this.type = MiltipleCrcBlockIO(channels).asInstanceOf[this.type]
}
object MiltipleCrcBlockIO {
  def apply(channels: Int): MiltipleCrcBlockIO = new MiltipleCrcBlockIO(channels)
}

abstract class MiltipleCrcBlock[D, U, E, O, B <: Data] (params: MiltipleCrcBlockParams, beatBytes: Int = 4) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {
  /* Type of Blocks */
  type Block = CrcChecker with CrcCheckerPins

  /* CRC 16 */
  lazy val crc16: Option[Block] = if (params.crcParams16 != None) Some(LazyModule(new CrcChecker(params.crcParams16.get) with CrcCheckerPins)) else None
  /* CRC 14 */
  lazy val crc14: Option[Block] = if (params.crcParams14 != None) Some(LazyModule(new CrcChecker(params.crcParams14.get) with CrcCheckerPins)) else None
  /* CRC 12 */
  lazy val crc12: Option[Block] = if (params.crcParams12 != None) Some(LazyModule(new CrcChecker(params.crcParams12.get) with CrcCheckerPins)) else None

  /* Seq of CRC modules */
  lazy val crcSeq = Seq(crc16, crc12, crc14)

  /* Number of different CRC modules */
  lazy val seqLength = crcSeq.flatten.length

  /* Control Seq */
  val controlSeq = crcSeq.zipWithIndex.filter(_._1 != None).map(_._2)

  /* Nodes */
  val streamNode = AXI4StreamIdentityNode()

  /* IO */
  lazy val io = Wire(new MiltipleCrcBlockIO(seqLength))

  lazy val module = new LazyModuleImp(this) {
    val in  = streamNode.in(0)._1
    val out = streamNode.out(0)._1

    /* control & status registers */
    val crcEnable      = RegInit(true.B)  /* by default CRC is enabled  */
    val passthroughCrc = RegInit(false.B)
    val crcErrorStatus = RegInit(0.U((seqLength).W))

    /* CRC should be implemented */
    if(seqLength >= 1) {
      /* Connect CRC 16 */
      crc16 match {
        case None => Nil
        case _ => {
          crc16.get.in.bits  := in.bits
          if (seqLength > 1) crc16.get.in.valid := in.valid && (io.word_size.get === 0.U) else crc16.get.in.valid := in.valid
          crc16.get.ioBlock.i_crcDataActive := io.crc_en.get
        }
      }
      /* Connect CRC 12 */
      crc12 match {
        case None => Nil
        case _ => {
          crc12.get.in.bits  := in.bits
          if (seqLength > 1) crc12.get.in.valid := in.valid && (io.word_size.get === 1.U) else crc12.get.in.valid := in.valid
          crc12.get.ioBlock.i_crcDataActive := io.crc_en.get
        }
      }
      /* Connect CRC 14 */
      crc14 match {
        case None => Nil
        case _ => {
          crc14.get.in.bits  := in.bits
          if (seqLength > 1) crc14.get.in.valid := in.valid && (io.word_size.get === 2.U) else crc14.get.in.valid := in.valid
          crc14.get.ioBlock.i_crcDataActive := io.crc_en.get
        }
      }
      /* Connect input ready and output valid */
      /* Default case for input ready, this will be overriden bellow */
      in.ready := 0.U
      /* Default case for output valid, this will be overriden bellow */
      out.valid := 0.U
      out.bits.data := 0.U
      out.bits.last := 0.U
      (crcSeq.flatten, controlSeq).zipped.map{ (crc, ctrl) => {
        if (seqLength > 1) {
          crc.ioBlock.i_crcEnable      := crcEnable
          crc.ioBlock.i_passthroughCrc := passthroughCrc
          when(io.word_size.get === ctrl.U) {in.ready := crc.in.ready}
          when(io.word_size.get === ctrl.U) {
            out.valid := crc.out.valid
            out.bits  := crc.out.bits
            crc.out.ready := out.ready
          }
        }
        else {
          in.ready := crc.in.ready
          out.valid := crc.out.valid
          out.bits  := crc.out.bits
          crc.out.ready := out.ready
        }
      }}
    }
    /* Passthrough, CRC shouldn't be implemented */
    else {
      out.bits  := in.bits
      out.valid := in.valid
      in.ready := out.ready
    }

    /* CRC status flag */
    crcErrorStatus := Cat(crcSeq.flatten.map{m => m.ioBlock.o_crcErrorStatus})

    /* define fields */
    val fields = Seq(
      RegField(1, crcEnable, RegFieldDesc(name = "crcEnable", desc = "Enable crc checker")),
      RegField(1, passthroughCrc, RegFieldDesc(name = "passtroughCrc", desc = "If this register is active CRC is not removed from stream")),
      RegField.r(seqLength, crcErrorStatus, RegFieldDesc(name = "crcErrorStatus", desc = "Status register used for crc error detection"))
    )

    /* Define abstract register map so it can be AXI4, Tilelink, APB, AHB */
    regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
  }
}

class AXI4MiltipleCrcBlock(params: MiltipleCrcBlockParams, address: AddressSet, beatBytes: Int = 4)(implicit p: Parameters) extends MiltipleCrcBlock[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
}

trait MiltipleCrcBlockPins extends AXI4MiltipleCrcBlock {

  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
      BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
      ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  // streamNode
  val ioInNode  = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := ioInNode
  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }

  // pins
  def makeCustomIO(): MiltipleCrcBlockIO = {
    val io2: MiltipleCrcBlockIO = IO(io.cloneType)
    io2.suggestName("io")
    io2 <> io
    io2
  }
  val ioBlock = InModuleBody { makeCustomIO() }
}

object MiltipleCrcBlockApp extends App
{
  val params = MiltipleCrcBlockParams(
  crcParams16 = Some(RadarCRCParams(dataWidth = 16)),
  crcParams12 = Some(RadarCRCParams(dataWidth = 12)),
  crcParams14 = None,//Some(RadarCRCParams(dataWidth = 14)),
)
  implicit val p: Parameters = Parameters.empty
  val lazyDut = LazyModule(new AXI4MiltipleCrcBlock(params, AddressSet(0x0000, 0xFF), 4) with MiltipleCrcBlockPins)
  (new ChiselStage).execute(Array("--target-dir", "verilog/AXI4MiltipleCrcBlock"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}
