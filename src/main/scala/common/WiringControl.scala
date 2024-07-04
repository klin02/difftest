/***************************************************************************************
 * Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
 *
 * DiffTest is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package difftest.common

import chisel3._
import chisel3.util.experimental.BoringUtils
import chisel3.experimental.ExtModule
import chisel3.util.HasExtModuleInline
import difftest.gateway.GatewayConfig

// Wrapper for the Chisel wiring utils.
private object WiringControl {
  def addSource(data: Data, name: String, isImplicit: Boolean): Unit = {
    if (isImplicit) {
      val source = Module(new WiringSource(data, name))
      source.io := data
    } else {
      BoringUtils.addSource(data, s"difftest_$name")
    }
  }

  def addSink(data: Data, name: String, isImplicit: Boolean): Unit = {
    if (isImplicit) {
      val sink = Module(new WiringSink(data, name)).suggestName(name)
      data := sink.io
    } else {
      BoringUtils.addSink(data, s"difftest_$name")
    }
  }
}

abstract class WiringBase(data: Data, name: String, isSource: Boolean) extends ExtModule with HasExtModuleInline {
  val io = {
    def do_direction(dt: Data): Data = if (isSource) Input(dt) else Output(dt)
    IO(do_direction(chiselTypeOf(data)))
  }
  override def desiredName: String = {
    val wiringType = if (isSource) "Source" else "Sink"
    s"Difftest${wiringType}_${name}"
  }

  def modPorts: Seq[(String, Data)] = data match {
    case b: Bundle => b.elements.toSeq.reverse.map { case (name, d) =>
      d match {
        case vec: Vec[_] => vec.zipWithIndex.map { case (v, i) => (s"io_${name}_$i", v) }
        case _ => Seq((s"io_$name", d))
      }
    }.flatten
    case _: UInt => Seq(("io", data))
  }

  def getModArgString(argName: String, data: Data): String = {
    val widthString = if (data.getWidth == 1) "      " else f"[${data.getWidth - 1}%2d:0]"
    val direction = if (isSource) "input" else "output"
    s"$direction $widthString $argName"
  }

  def moduleAssign: String = ""
  def moduleBody: String = {
    val modPortsString = modPorts.map(i => getModArgString(i._1, i._2)).mkString(",\n  ")
    s"""
       |module $desiredName(
       |  $modPortsString
       |);
       |$moduleAssign
       |endmodule
       |""".stripMargin
  }
}

class WiringSource(data: Data, name: String) extends WiringBase(data, name, true) {
  override def moduleAssign: String = {
    s"""
       |`ifndef SYNTHESIS
       |`ifdef DIFFTEST
       |  assign SimTop.difftest_sink.${name}.io = io;
       |`endif
       |`endif
       |""".stripMargin
  }
  setInline(s"$desiredName.v", moduleBody)
}

class WiringSink(data: Data, name: String) extends WiringBase(data, name, false) {
  setInline(s"$desiredName.v", moduleBody)
}

private class WiringInfo(val dataType: Data, val name: String) {
  private var nSources: Int = 0
  private var nSinks: Int = 0

  def setSource(): WiringInfo = {
    require(nSources == 0, s"$name already declared as a source")
    nSources += 1
    this
  }

  def addSink(): WiringInfo = {
    nSinks += 1
    this
  }

  def isPending: Boolean = nSources == 0 || nSinks == 0
  // (isSource, dataType, name)
  def toPendingTuple: Option[(Boolean, Data, String)] = {
    if (isPending) {
      Some((nSources == 0, dataType, name))
    } else {
      None
    }
  }
}

object DifftestWiring {
  private val wires = scala.collection.mutable.ListBuffer.empty[WiringInfo]
  private def getWire(data: Data, name: String): WiringInfo = {
    wires.find(_.name == name).getOrElse {
      val info = new WiringInfo(chiselTypeOf(data), name)
      wires.addOne(info)
      info
    }
  }

  def addSource(data: Data, name: String, config: GatewayConfig = GatewayConfig()): Data = {
    getWire(data, name).setSource()
    WiringControl.addSource(data, name, config.implicitWiring)
    data
  }

  def addSink[T <: Data](data: T, name: String, config: GatewayConfig = GatewayConfig()): T = {
    getWire(data, name).addSink()
    WiringControl.addSink(data, name, config.implicitWiring)
    data
  }

  def isEmpty: Boolean = !hasPending
  def hasPending: Boolean = wires.exists(_.isPending)
  def getPending: Seq[(Boolean, Data, String)] = wires.flatMap(_.toPendingTuple).toSeq

  def createPendingWires(): Seq[Data] = {
    getPending.map { case (isSource, dataType, name) =>
      val target = WireInit(0.U.asTypeOf(dataType)).suggestName(name)
      if (isSource) {
        addSource(target, name)
      } else {
        addSink(target, name)
      }
    }
  }

  def createExtraIOs(flipped: Boolean = false): Seq[Data] = {
    getPending.map { case (isSource, dataType, name) =>
      def do_direction(dt: Data): Data = if (isSource) Input(dt) else Output(dt)
      def do_flip(dt: Data): Data = if (flipped) Flipped(dt) else dt
      IO(do_flip(do_direction(dataType))).suggestName(name)
    }
  }

  def createAndConnectExtraIOs(): Seq[Data] = {
    createExtraIOs().zip(createPendingWires()).map { case (io, wire) =>
      io <> wire
      io
    }
  }
}
