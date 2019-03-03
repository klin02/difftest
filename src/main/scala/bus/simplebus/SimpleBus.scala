package bus.simplebus

import chisel3._
import chisel3.util._

import utils._
import bus.axi4._

class SimpleBusReqBundle(val dataBits: Int) extends Bundle {
  val addr = Output(UInt(32.W))
  val size = Output(UInt(3.W))
  val wen = Output(Bool())
  val wmask = Output(UInt((dataBits / 8).W))
  val wdata = Output(UInt(dataBits.W))

  override def toPrintable: Printable = {
    p"addr = 0x${Hexadecimal(addr)}, size = 0x${Hexadecimal(size)}, " +
    p"wen = ${wen}, wmask = 0x${Hexadecimal(wmask)}, wdata = 0x${Hexadecimal(wdata)}"
  }
}

class SimpleBusRespBundle(val dataBits: Int) extends Bundle {
  val rdata = Output(UInt(dataBits.W))

  override def toPrintable: Printable = {
    p"rdata = ${Hexadecimal(rdata)}"
  }
}

class SimpleBus(val dataBits: Int = 32) extends Bundle {
  val req = Decoupled(new SimpleBusReqBundle(dataBits))
  val resp = Flipped(Decoupled(new SimpleBusRespBundle(dataBits)))

  def isWrite() = req.valid && req.bits.wen
  def isRead() = req.valid && !req.bits.wen

  def toAXI4(isLite: Boolean = false) = {
    val mem2axi = Module(new SimpleBus2AXI4Converter(if (isLite) new AXI4Lite else new AXI4))
    mem2axi.io.in <> this
    mem2axi.io.out
  }

  def dump(name: String) = {
    when (req.fire()) { printf(p"${GTimer()},[${name}] ${req.bits}\n") }
    when (resp.fire()) { printf(p"${GTimer()},[${name}] ${resp.bits}\n") }
  }
}