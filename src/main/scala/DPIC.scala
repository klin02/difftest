/***************************************************************************************
 * Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
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

package difftest.dpic

import chisel3._
import chisel3.experimental.ExtModule
import chisel3.reflect.DataMirror
import chisel3.util._
import difftest.DifftestModule.streamToFile
import difftest._
import difftest.gateway.{GatewayBatchBundle, GatewayBundle, GatewayConfig, GatewayResult}

import scala.collection.mutable.ListBuffer

class DPIC[T <: DifftestBundle](gen: T, config: GatewayConfig)
  extends ExtModule
  with HasExtModuleInline
  with DifftestModule[T] {
  val clock = IO(Input(Clock()))
  val enable = IO(Input(Bool()))
  val io = IO(Input(gen))
  val dut_zone = Option.when(config.hasDutZone)(IO(Input(UInt(config.dutZoneWidth.W))))
  val dut_index = Option.when(config.isBatch)(IO(Input(UInt(log2Ceil(config.batchSize).W))))

  def getDirectionString(data: Data): String = {
    if (DataMirror.directionOf(data) == ActualDirection.Input) "input " else "output"
  }

  def getDPICArgString(argName: String, data: Data, isC: Boolean): String = {
    val typeString = data.getWidth match {
      case 1                                  => if (isC) "uint8_t" else "bit"
      case width if width > 1 && width <= 8   => if (isC) "uint8_t" else "byte"
      case width if width > 8 && width <= 32  => if (isC) "uint32_t" else "int"
      case width if width > 32 && width <= 64 => if (isC) "uint64_t" else "longint"
      case _                                  => s"unsupported io type of width ${data.getWidth}!!\n"
    }
    if (isC) {
      f"$typeString%-8s $argName"
    } else {
      val directionString = getDirectionString(data)
      f"$directionString $typeString%8s $argName"
    }
  }

  def getModArgString(argName: String, data: Data): String = {
    val widthString = if (data.getWidth == 1) "      " else f"[${data.getWidth - 1}%2d:0]"
    val argString = Seq(getDirectionString(data), widthString, s"$argName")
    argString.mkString(" ")
  }

  override def desiredName: String = gen.desiredModuleName
  val dpicFuncName: String = s"v_difftest_${desiredName.replace("Difftest", "")}"
  val modPorts: Seq[Seq[(String, Data)]] = {
    val common = ListBuffer(Seq(("clock", clock)), Seq(("enable", enable)))
    if (config.hasDutZone) common += Seq(("dut_zone", dut_zone.get))
    if (config.isBatch) common += Seq(("dut_index", dut_index.get))
    // ExtModule implicitly adds io_* prefix to the IOs (because the IO val is named as io).
    // This is different from BlackBoxes.
    common.toSeq ++ io.elements.toSeq.reverse.map { case (name, data) =>
      data match {
        case vec: Vec[_] => vec.zipWithIndex.map { case (v, i) => (s"io_${name}_$i", v) }
        case _           => Seq((s"io_$name", data))
      }
    }
  }
  val dpicFuncArgsWithClock = if (gen.bits.hasValid) {
    modPorts.filterNot(p => p.length == 1 && p.head._1 == "io_valid")
  } else modPorts
  val dpicDropNum = 2
  val dpicFuncArgs: Seq[Seq[(String, Data)]] = dpicFuncArgsWithClock.drop(2)
  val dpicFuncAssigns: Seq[String] = {
    val filters: Seq[(DifftestBundle => Boolean, Seq[String])] = Seq(
      ((_: DifftestBundle) => true, Seq("io_coreid")),
      ((_: DifftestBundle) => config.hasDutZone, Seq("dut_zone")),
      ((_: DifftestBundle) => config.isBatch, Seq("dut_index")),
      ((x: DifftestBundle) => x.isIndexed, Seq("io_index")),
      ((x: DifftestBundle) => x.isFlatten, Seq("io_address")),
    )
    val rhs = dpicFuncArgs.map(_.map(_._1).filterNot(s => filters.exists(f => f._1(gen) && f._2.contains(s))))
    val lhs = rhs
      .map(_.map(_.replace("io_", "")))
      .flatMap(r =>
        if (r.length == 1) r
        else r.map(x => x.slice(0, x.lastIndexOf('_')) + s"[${x.split('_').last}]")
      )
    val body = lhs.zip(rhs.flatten).map { case (l, r) => s"packet->$l = $r;" }
    val validAssign = if (!gen.bits.hasValid || gen.isFlatten) Seq() else Seq("packet->valid = true;")
    validAssign ++ body
  }
  val dpicFuncProto: String =
    s"""
       |extern "C" void $dpicFuncName (
       |  ${dpicFuncArgs.flatten.map(arg => getDPICArgString(arg._1, arg._2, true)).mkString(",\n  ")}
       |)""".stripMargin
  val dpicFunc: String = {
    val dut_zone = if (config.hasDutZone) "dut_zone" else "0"
    val dut_index = if (config.isBatch) "dut_index" else "0"
    val packet = s"DUT_BUF(io_coreid, $dut_zone, $dut_index)->${gen.desiredCppName}"
    val index = if (gen.isIndexed) "[io_index]" else if (gen.isFlatten) "[io_address]" else ""
    s"""
       |$dpicFuncProto {
       |  if (!diffstate_buffer) return;
       |  auto packet = &($packet$index);
       |  ${dpicFuncAssigns.mkString("\n  ")}
       |}
       |""".stripMargin
  }

  val moduleBody: String = {
    val dpicDecl =
      // (1) DPI-C function prototype
      s"""
         |import "DPI-C" function void $dpicFuncName (
         |  ${dpicFuncArgs.flatten.map(arg => getDPICArgString(arg._1, arg._2, false)).mkString(",\n  ")}
         |);
         |""".stripMargin
    // (2) module definition
    val modPortsString = modPorts.flatten.map(i => getModArgString(i._1, i._2)).mkString(",\n  ")
    // Initial for Palladium GFIFO
    val gfifoInitial =
      if (config.isNonBlock)
        s"""
           |`ifdef PALLADIUM
           |initial $$ixc_ctrl("gfifo", "$dpicFuncName");
           |`endif
           |""".stripMargin
      else ""
    val modDef =
      s"""
         |module $desiredName(
         |  $modPortsString
         |);
         |`ifndef SYNTHESIS
         |`ifdef DIFFTEST
         |$dpicDecl
         |$gfifoInitial
         |  always @(posedge clock) begin
         |    if (enable)
         |    $dpicFuncName (${dpicFuncArgs.flatten.map(_._1).mkString(", ")});
         |  end
         |`endif
         |`endif
         |endmodule
         |""".stripMargin
    modDef
  }

  setInline(s"$desiredName.v", moduleBody)
}

class DPICBatch[T <: Seq[DifftestBundle]](template: T, bundle: GatewayBatchBundle, config: GatewayConfig)
  extends ExtModule
  with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val io = IO(Input(bundle))

  def getModArgString(argName: String, data: Data): String = {
    val widthString = if (data.getWidth == 1) "      " else f"[${data.getWidth - 1}%2d:0]"
    s"input $widthString $argName"
  }

  def getDPICArgString(argName: String, data: Data, isC: Boolean): String = {
    if (argName == "dut_zone") {
      if (isC) s"uint8_t $argName" else s"input byte $argName"
    } else {
      val byteLen = data.getWidth / 8
      if (isC) s"const svBitVecVal $argName[$byteLen]" else s"input bit[7:0] $argName[$byteLen]"
    }
  }

  def getDPICBundleUnpack(gen: DifftestBundle): String = {
    val unpack = ListBuffer.empty[String]
    def byteCnt(data: Data): Int = (data.getWidth + (8 - data.getWidth % 8) % 8) / 8
    case class ArgPair(name: String, width: Int, offset: Int, isVec: Boolean)
    val argsWithWidthOffset: Seq[ArgPair] = {
      val list = ListBuffer.empty[ArgPair]
      var offset: Int = 0
      for ((name, data) <- gen.elements.toSeq.reverse) {
        if (name != "valid") {
          data match {
            case vec: Vec[_] => {
              for ((v, i) <- vec.zipWithIndex) {
                list += ArgPair(s"${name}_$i", byteCnt(v), offset, true)
                offset += byteCnt(v)
              }
            }
            case d: Data => {
              list += ArgPair(name, byteCnt(d), offset, false)
              offset += byteCnt(d)
            }
          }
        }
      }
      list.toSeq
    }
    def varAssign(pair: ArgPair, prefix: String): String = {
      val rhs = (0 until pair.width).map { i =>
        val appendOffset = if (i != 0) s" << ${8 * i}" else ""
        s"(uint${pair.width * 8}_t)data[offset + ${pair.offset + i}]$appendOffset"
      }.mkString(" |\n        ")
      val lhs = (pair.name, pair.isVec) match {
        case (x, true)  => x.slice(0, x.lastIndexOf('_')) + s"[${x.split('_').last}]"
        case (x, false) => x
      }
      s"$prefix$lhs = $rhs;"
    }
    val filterIn = ListBuffer("coreid")
    val index = if (gen.isIndexed) {
      filterIn += "index"
      "[index]"
    } else if (gen.isFlatten) {
      filterIn += "address"
      "[address]"
    } else {
      ""
    }
    unpack ++= argsWithWidthOffset.filter(p => filterIn.contains(p.name)).map(p => varAssign(p, ""))
    val dut_zone = if (config.hasDutZone) "dut_zone" else "0"
    val packet = s"DUT_BUF(coreid, $dut_zone, 0)->${gen.desiredCppName}"
    unpack += s"auto packet = &($packet$index);"
    if (gen.bits.hasValid && !gen.isFlatten) unpack += "packet->valid = true;"
    val filterOut = Seq("coreid", "index", "address", "valid")
    unpack ++= argsWithWidthOffset.filterNot(p => filterOut.contains(p.name)).map(p => varAssign(p, "packet->"))
    unpack.toSeq.mkString("\n      ")
  }

  override def desiredName: String = "DifftestBatch"
  val dpicFuncName: String = s"v_difftest_${desiredName.replace("Difftest", "")}"
  val dpicFuncArgs: Seq[(String, Data)] = {
    val common = Seq(("data", io.data), ("info", io.info))
    if (config.hasDutZone) common ++ Seq(("dut_zone", io.dut_zone.get)) else common
  }
  val dpicFuncProto: String =
    s"""
       |extern "C" void $dpicFuncName (
       |  ${dpicFuncArgs.map(arg => getDPICArgString(arg._1, arg._2, true)).mkString(",\n  ")}
       |)""".stripMargin
  val dpicFunc: String = {
    val bundleEnum = template.map(_.desiredModuleName.replace("Difftest", ""))
    val bundleAssign = template.zipWithIndex.map { case (t, idx) =>
      val head = if (idx == 0) "if" else "else if"
      s"""
         |    $head (id == ${bundleEnum(idx)}) {
         |      ${getDPICBundleUnpack(t)}
         |    }
        """.stripMargin
    }.mkString("")
    val infoLen = io.info.getWidth / 8
    s"""
       |enum DifftestBundle {
       |  ${bundleEnum.mkString(",\n  ")}
       |};
       |$dpicFuncProto {
       |  if (!diffstate_buffer) return;
       |  for (int i = 0; i < $infoLen; i+=3) {
       |    uint8_t id = info[i+2];
       |    uint32_t offset = (info[i+1] << 8) | info[i];
       |    uint32_t coreid, index, address;
       |    $bundleAssign
       |  }
       |}
       |""".stripMargin
  }

  val modPorts: Seq[(String, Data)] = {
    Seq(("clock", clock)) ++ io.elements.toSeq.reverse.map { case (name, data) => (s"io_$name", data) }
  }
  val moduleBody: String = {
    val packArgs = Seq(("data", io.data), ("info", io.info))
    val bytePack = packArgs.map { case (name, data) =>
      val len = data.getWidth / 8
      s"""
         |bit[7:0] $name[$len];
         |for (genvar i = 0; i < $len; i++) begin
         |  assign $name[i] = io_$name[i * 8 +: 8];
         |end
         |""".stripMargin
    }.mkString("")
    val dpicDecl =
      s"""
         |import "DPI-C" function void $dpicFuncName (
         |  ${dpicFuncArgs.map(arg => getDPICArgString(arg._1, arg._2, false)).mkString(",\n  ")}
         |);
         |""".stripMargin
    val gfifoInitial =
      if (config.isNonBlock)
        s"""
           |`ifdef PALLADIUM
           |initial $$ixc_ctrl("gfifo", "$dpicFuncName");
           |`endif
           |""".stripMargin
      else ""
    val modPortsString = modPorts.map(i => getModArgString(i._1, i._2)).mkString(",\n  ")
    val dpicFuncParam = dpicFuncArgs.map { case (name, _) => if (packArgs.exists(_._1 == name)) name else s"io_$name"}
    s"""
       |`include "DifftestMacros.v"
       |module $desiredName(
       |  $modPortsString
       |);
       |`ifndef SYNTHESIS
       |`ifdef DIFFTEST
       |$bytePack
       |$dpicDecl
       |$gfifoInitial
       |  always @(posedge clock) begin
       |    if (io_enable)
       |      $dpicFuncName (${dpicFuncParam.mkString(", ")});
       |  end
       |`endif
       |`endif
       |endmodule
       |""".stripMargin
  }

  setInline(s"$desiredName.v", moduleBody)
}

private class DummyDPICWrapper[T <: DifftestBundle](gen: T, config: GatewayConfig) extends Module {
  val io = IO(Input(new GatewayBundle(gen, config)))
  val dpic = Module(new DPIC(gen, config))
  dpic.clock := clock
  dpic.enable := io.data.bits.getValid && io.enable
  dpic.io := io.data
  if (config.hasDutZone) dpic.dut_zone.get := io.dut_zone.get
}

private class DummyDPICBatchWrapper[T <: Seq[DifftestBundle]](
  template: T,
  bundle: GatewayBatchBundle,
  config: GatewayConfig,
) extends Module {
  val io = IO(Input(bundle))
  val dpic = Module(new DPICBatch(template, bundle, config))
  dpic.clock := clock
  dpic.io := io
}

object DPIC {
  val interfaces = ListBuffer.empty[(String, String, String)]

  def apply(bundle: GatewayBundle, config: GatewayConfig): Unit = {
    val module = Module(new DummyDPICWrapper(bundle.data.cloneType, config))
    module.io := bundle
    val dpic = module.dpic
    if (!interfaces.map(_._1).contains(dpic.dpicFuncName)) {
      val interface = (dpic.dpicFuncName, dpic.dpicFuncProto, dpic.dpicFunc)
      interfaces += interface
    }
  }

  def batch(template: MixedVec[DifftestBundle], bundle: GatewayBatchBundle, config: GatewayConfig): Unit = {
    val module = Module(new DummyDPICBatchWrapper(template.toSeq.map(_.cloneType), bundle.cloneType, config))
    module.io := bundle
    val dpic = module.dpic
    interfaces += ((dpic.dpicFuncName, dpic.dpicFuncProto, dpic.dpicFunc))
  }

  def collect(): GatewayResult = {
    if (interfaces.isEmpty) {
      return GatewayResult()
    }

    val interfaceCpp = ListBuffer.empty[String]
    interfaceCpp += "#ifndef __DIFFTEST_DPIC_H__"
    interfaceCpp += "#define __DIFFTEST_DPIC_H__"
    interfaceCpp += ""
    interfaceCpp += "#include <cstdint>"
    interfaceCpp += "#include \"diffstate.h\""
    interfaceCpp += "#ifdef CONFIG_DIFFTEST_BATCH"
    interfaceCpp += "#include \"svdpi.h\""
    interfaceCpp += "#endif // CONFIG_DIFFTEST_BATCH"
    interfaceCpp += ""
    interfaceCpp +=
      """
        |class DPICBuffer : public DiffStateBuffer {
        |private:
        |  DiffTestState buffer[CONFIG_DIFFTEST_ZONESIZE][CONFIG_DIFFTEST_BUFLEN];
        |  int read_ptr = 0;
        |  int zone_ptr = 0;
        |  bool init = true;
        |public:
        |  DPICBuffer() {
        |    memset(buffer, 0, sizeof(buffer));
        |  }
        |  inline DiffTestState* get(int zone, int index) {
        |    return buffer[zone] + index;
        |  }
        |  inline DiffTestState* next() {
        |    DiffTestState* ret = buffer[zone_ptr] + read_ptr;
        |    read_ptr = read_ptr + 1;
        |    return ret;
        |  }
        |  inline void switch_zone() {
        |    if (init) {
        |      init = false;
        |      return;
        |    }
        |    zone_ptr = (zone_ptr + 1) % CONFIG_DIFFTEST_ZONESIZE;
        |    read_ptr = 0;
        |  }
        |};
        |""".stripMargin
    interfaceCpp += interfaces.map(_._2 + ";").mkString("\n")
    interfaceCpp += ""
    interfaceCpp += "#endif // __DIFFTEST_DPIC_H__"
    interfaceCpp += ""
    streamToFile(interfaceCpp, "difftest-dpic.h")

    interfaceCpp.clear()
    interfaceCpp += "#ifndef CONFIG_NO_DIFFTEST"
    interfaceCpp += ""
    interfaceCpp += "#include \"difftest.h\""
    interfaceCpp += "#include \"difftest-dpic.h\""
    interfaceCpp += ""
    interfaceCpp +=
      s"""
         |DiffStateBuffer** diffstate_buffer = nullptr;
         |#define DUT_BUF(core_id, zone, index) (diffstate_buffer[core_id]->get(zone, index))
         |
         |void diffstate_buffer_init() {
         |  diffstate_buffer = new DiffStateBuffer*[NUM_CORES];
         |  for (int i = 0; i < NUM_CORES; i++) {
         |    diffstate_buffer[i] = new DPICBuffer;
         |  }
         |}
         |
         |void diffstate_buffer_free() {
         |  for (int i = 0; i < NUM_CORES; i++) {
         |    delete diffstate_buffer[i];
         |  }
         |  delete[] diffstate_buffer;
         |  diffstate_buffer = nullptr;
         |}
      """.stripMargin
    interfaceCpp += interfaces.map(_._3).mkString("")
    interfaceCpp += ""
    interfaceCpp += "#endif // CONFIG_NO_DIFFTEST"
    interfaceCpp += ""
    streamToFile(interfaceCpp, "difftest-dpic.cpp")

    GatewayResult(
      cppMacros = Seq("CONFIG_DIFFTEST_DPIC"),
      step = Some(1.U),
    )
  }
}
