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

case class MiltipleCrcParams(
  crcParams16 : Option[RadarCRCParams],
  crcParams12 : Option[RadarCRCParams],
  crcParams14 : Option[RadarCRCParams],
  channels    : Int,
)
// MiltipleCrc Bundle
class MiltipleCrcIO(channels: Int) extends Bundle {
    val word_size = if (channels > 1) Some(Input(UInt(2.W))) else None
    val crc_en    = if (channels >= 1) Some(Input(UInt(1.W))) else None

    override def cloneType: this.type = MiltipleCrcIO(channels).asInstanceOf[this.type]
}
object MiltipleCrcIO {
  def apply(channels: Int): MiltipleCrcIO = new MiltipleCrcIO(channels)
}

abstract class MiltipleCrc(params: MiltipleCrcParams, beatBytes: Int = 4) extends LazyModule()(Parameters.empty) with HasCSR {
  /* Type of Blocks */
  type Block = CrcChecker with CrcCheckerPins

  /* CRC 16 */
  lazy val crc16: Option[Seq[Block]] = if (params.crcParams16 != None) Some(Seq.fill(params.channels){LazyModule(new CrcChecker(params.crcParams16.get) with CrcCheckerPins)}) else None
  /* CRC 14 */
  lazy val crc14: Option[Seq[Block]] = if (params.crcParams14 != None) Some(Seq.fill(params.channels){LazyModule(new CrcChecker(params.crcParams14.get) with CrcCheckerPins)}) else None
  /* CRC 12 */
  lazy val crc12: Option[Seq[Block]] = if (params.crcParams12 != None) Some(Seq.fill(params.channels){LazyModule(new CrcChecker(params.crcParams12.get) with CrcCheckerPins)}) else None

  /* Seq of CRC modules */
  lazy val crcSeq = Seq(crc16, crc12, crc14)

  /* Number of different CRC modules */
  lazy val seqLength = crcSeq.flatten.length

  /* Control Seq */
  val controlSeq = crcSeq.zipWithIndex.filter(_._1 != None).map(_._2)

  /* Nodes */
  val streamNode: Seq[AXI4StreamIdentityNode] = Seq.fill(params.channels){AXI4StreamIdentityNode()}

  /* IO */
  lazy val io = Wire(new MiltipleCrcIO(seqLength))

  lazy val module = new LazyModuleImp(this) {
    val in  = streamNode.map(m => m.in(0)._1)
    val out = streamNode.map(m => m.out(0)._1)

    /* control & status registers */
    val crcEnable      = RegInit(true.B)  /* by default CRC is enabled  */
    val passthroughCrc = RegInit(false.B)
    val crcErrorStatus = RegInit(0.U((if (seqLength >= 1) seqLength*params.channels else params.channels).W))

    /* CRC should be implemented */
    if(seqLength >= 1) {
      /* Connect CRC 16 */
      crc16 match {
        case None => Nil
        case _ => crc16.get.zipWithIndex.map{ case (m, i) =>
          m.in.bits  := in(i).bits
          if (seqLength > 1) m.in.valid := in(i).valid && (io.word_size.get === 0.U) else m.in.valid := in(i).valid
          m.ioBlock.i_crcDataActive := io.crc_en.get
        }
      }
      /* Connect CRC 12 */
      crc12 match {
        case None => Nil
        case _ => crc12.get.zipWithIndex.map{ case (m, i) =>
          m.in.bits  := in(i).bits
          if (seqLength > 1) m.in.valid := in(i).valid && (io.word_size.get === 1.U) else m.in.valid := in(i).valid
          m.ioBlock.i_crcDataActive := io.crc_en.get
        }
      }
      /* Connect CRC 14 */
      crc14 match {
        case None => Nil
        case _ => crc12.get.zipWithIndex.map{ case (m, i) =>
          m.in.bits  := in(i).bits
          if (seqLength > 1) m.in.valid := in(i).valid && (io.word_size.get === 2.U) else m.in.valid := in(i).valid
          m.ioBlock.i_crcDataActive := io.crc_en.get
        }
      }
      /* Connect input ready and output valid */
      /* Default case for input ready, this will be overriden bellow */
      in.zipWithIndex.map {case (input, i) =>
        input.ready := 0.U
      }
      /* Default case for output valid, this will be overriden bellow */
      out.zipWithIndex.map {case (output, i) =>
        output.valid := 0.U
        output.bits.data := 0.U
        output.bits.last := 0.U
      }
      (crcSeq.flatten, controlSeq).zipped.map{ (crc, ctrl) => {
        if (seqLength > 1) {
          crc.map(m => {
            m.ioBlock.i_crcEnable      := crcEnable
            m.ioBlock.i_passthroughCrc := passthroughCrc
          })
          in.zipWithIndex.map {case (input, i) =>
            when(io.word_size.get === ctrl.U) {input.ready := crc(i).in.ready}
          }
          out.zipWithIndex.map {case (output, i) =>
            when(io.word_size.get === ctrl.U) {
              output.valid := crc(i).out.valid
              output.bits  := crc(i).out.bits
              crc(i).out.ready := output.ready
            }
          }
        }
        else {
          in.zipWithIndex.map {case (input, i) =>
            input.ready := crc(i).in.ready
          }
          out.zipWithIndex.map {case (output, i) =>
            output.valid := crc(i).out.valid
            output.bits  := crc(i).out.bits
            crc(i).out.ready := output.ready
          }
        }
      }}
    }
    /* Passthrough, CRC shouldn't be implemented */
    else {
      (in, out).zipped.map{ (i, o) => {
        o.bits  := i.bits
        o.valid := i.valid
        i.ready := o.ready
      }}
    }

    /* CRC status flag */
    crcErrorStatus := Cat(crcSeq.flatten.flatten.map{m => m.ioBlock.o_crcErrorStatus})

    /* define fields */
    val fields = Seq(
      RegField(1, crcEnable, RegFieldDesc(name = "crcEnable", desc = "Enable crc checker")),
      RegField(1, passthroughCrc, RegFieldDesc(name = "passtroughCrc", desc = "If this register is active CRC is not removed from stream")),
      RegField.r(if (seqLength >= 1) seqLength*params.channels else params.channels, crcErrorStatus, RegFieldDesc(name = "crcErrorStatus", desc = "Status register used for crc error detection"))
    )

    /* Define abstract register map so it can be AXI4, Tilelink, APB, AHB */
    regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
  }
}

class AXI4MiltipleCrc(params: MiltipleCrcParams, address: AddressSet, beatBytes: Int = 4)(implicit p: Parameters) extends MiltipleCrc(params, beatBytes) {
  /* override val mem: Some[AXI4RegisterNode] */
  val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
  override def regmap(mapping: (Int, Seq[RegField])*): Unit = mem.get.regmap(mapping:_*)
}

trait MiltipleCrcPins extends AXI4MiltipleCrc {

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
  val streamLen = streamNode.length

  val ioInNode  = Seq.fill(streamLen){BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))}
  val ioOutNode = Seq.fill(streamLen){BundleBridgeSink[AXI4StreamBundle]()}

  val streamPins = for (i <- 0 until streamLen) yield {
    ioOutNode(i) := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode(i) := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := ioInNode(i)
    val in = InModuleBody { 
      implicit val valName = ValName(s"in_$i")
      ioInNode(i).makeIO()
    }
    val out = InModuleBody { 
      implicit val valName = ValName(s"out_$i")
      ioOutNode(i).makeIO()
    }
    (in,out)
  }

  // pins
  def makeCustomIO(): MiltipleCrcIO = {
    val io2: MiltipleCrcIO = IO(io.cloneType)
    io2.suggestName("io")
    io2 <> io
    io2
  }
  val ioBlock = InModuleBody { makeCustomIO() }
}

object MiltipleCrcApp extends App
{
  val params = MiltipleCrcParams(
  crcParams16 = Some(RadarCRCParams(dataWidth = 16)),
  crcParams12 = Some(RadarCRCParams(dataWidth = 12)),
  crcParams14 = None,//Some(RadarCRCParams(dataWidth = 14)),
  channels    = 2,
)
  implicit val p: Parameters = Parameters.empty
  val lazyDut = LazyModule(new AXI4MiltipleCrc(params, AddressSet(0x0000, 0xFF), 4) with MiltipleCrcPins)
  (new ChiselStage).execute(Array("--target-dir", "verilog/AXI4MiltipleCrc"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}
