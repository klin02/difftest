/***************************************************************************************
 * Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
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

package difftest.delta

import chisel3._
import chisel3.util._
import difftest._
import difftest.common.FileControl
import difftest.gateway.GatewayConfig
import difftest.util.PipelineConnect

import scala.collection.mutable.ListBuffer

object Delta {
  private val instances = ListBuffer.empty[DifftestBundle]
  def apply(
    bundles: DecoupledIO[MixedVec[Valid[DifftestBundle]]],
    config: GatewayConfig,
  ): DecoupledIO[MixedVec[Valid[DifftestBundle]]] = {
    instances ++= bundles.bits.map(_.bits)
    val module = Module(new DeltaEndpoint(chiselTypeOf(bundles.bits).toSeq, config))
    module.in <> bundles
    module.out
  }
  def collect(): Unit = {
    val deltaCpp = ListBuffer.empty[String]
    val deltaInsts = instances.filter(_.supportsDelta).distinct
    val deltaDecl = deltaInsts.map { inst =>
      val len = inst.dataElements.flatMap(_._3).length
      val elemType = s"uint${inst.deltaElemWidth}_t"
      s"$elemType ${inst.desiredCppName}_elem[$len];"
    }

    deltaCpp += "#ifndef __DIFFTEST_DELTA_H__"
    deltaCpp += "#define __DIFFTEST_DELTA_H__"
    deltaCpp += "#include \"difftest-state.h\""
    deltaCpp += (new DiffDeltaInfo).toCppDeclaration(true, true)
    deltaCpp +=
      s"""
         |typedef struct {
         |  DifftestDeltaInfo delta_info;
         |  ${deltaDecl.mkString("\n  ")}
         |} DeltaState;
         |""".stripMargin

    def deltaSync(dst: String, src: String): Seq[String] = {
      deltaInsts.map { inst =>
        val destName = inst.actualCppName
        val srcName = inst.desiredCppName
        s"memcpy(&($dst->$destName), $src->${srcName}_elem, sizeof(${inst.desiredModuleName}));"
      }.toSeq
    }
    deltaCpp +=
      s"""
         |class DeltaStats {
         |private:
         |  DeltaState buffer[NUM_CORES];
         |public:
         |  bool hasProgress = false;
         |
         |  DeltaStats() {
         |    memset(buffer, 0, sizeof(buffer));
         |  }
         |  DeltaState* get(int coreid){
         |    return buffer + coreid;
         |  }
         |  bool need_pending() {
         |    return hasProgress && !get(0)->delta_info.valid;
         |  }
         |  void sync(int zone, int index) {
         |    for (int i = 0; i < NUM_CORES; i++) {
         |      DiffTestState* dut = diffstate_buffer[i]->get(zone, index);
         |      DeltaState* delta = get(i);
         |      ${deltaSync("dut", "delta").mkString("\n      ")}
         |    }
         |    hasProgress = false;
         |    get(0)->delta_info.valid = false;
         |  }
         |};
         |""".stripMargin
    deltaCpp += "#endif // __DIFFTEST_DELTA_H__"
    FileControl.write(deltaCpp, "difftest-delta.h")
  }
}

class DeltaSplitter(v_gen: Valid[DifftestBundle], filter: Option[UInt]) extends Module {
  val in = IO(Input(v_gen))
  val in_filter = Option.when(filter.isDefined)(IO(Input(chiselTypeOf(filter.get))))
  val accept = IO(Input(Bool()))

  val first_elems = VecInit(in.bits.dataElements.flatMap(_._3))
  val r_elems = RegInit(0.U.asTypeOf(first_elems))
  val out = IO(Output(Vec(first_elems.length, Valid(new DiffDeltaElem(v_gen.bits)))))

  val update_mask = in_filter.getOrElse(Fill(first_elems.length, true.B)).asBools
  val first_updates = VecInit(first_elems.zip(r_elems).zip(update_mask).map { case ((e, s), m) =>
    e =/= s && in.valid && m
  })
  r_elems.zip(first_elems).zip(first_updates).map { case ((r, e), u) =>
    when(accept && u) {
      r := e
    }
  }

  out.zipWithIndex.foreach { case (delta, idx) =>
    delta.valid := first_updates(idx)
    delta.bits.coreid := in.bits.coreid
    delta.bits.index := idx.U
    delta.bits.data := first_elems(idx)
  }
}

class DeltaEndpoint(bundles: Seq[Valid[DifftestBundle]], config: GatewayConfig) extends Module {
  val in = IO(Flipped(Decoupled(MixedVec(bundles))))
  val pipelined = Wire(Decoupled(MixedVec(bundles)))
  PipelineConnect(in, pipelined, pipelined.fire)

  val toDeltas = pipelined.bits.filter(_.bits.supportsDelta)

  val splitters = toDeltas.map { v_gen =>
    val filter: Option[UInt] = v_gen.bits match {
      case preg: DiffPhyRegState =>
        Option.when(preg.needRat) {
          val filterWidth = preg.numPhyRegs
          pipelined.bits.map { v_gen =>
            val res = v_gen.bits match {
              case rat: DiffArchRenameTable if rat.desiredCppName == preg.ratTarget.desiredCppName => {
                rat.value.map { regIdx => UIntToOH(regIdx, filterWidth) }.reduce(_ | _)
              }
              case cmt: DiffInstrCommit => {
                // Only check vecCommit when multi-core load
                val wpdest = if (bundles.exists(_.bits.desiredCppName == "load")) {
                  cmt.otherwpdest ++ Seq(cmt.wpdest)
                } else {
                  Seq(cmt.wpdest)
                }
                wpdest.map { dst => UIntToOH(dst, filterWidth) }.reduce(_ | _)
              }
              case _ => false.B
            }
            Mux(v_gen.valid, res, 0.U(filterWidth.W))
          }.reduce(_ | _)
        }
      case _ => None
    }

    val module = Module(new DeltaSplitter(chiselTypeOf(v_gen), filter))
    module.in.bits := v_gen.bits
    module.in.valid := v_gen.valid && pipelined.valid
    module.in_filter.foreach(_ := filter.get)
    module.accept := pipelined.fire
    module
  }
  val deltas = splitters.flatMap(_.out)

  val deltaInfo = Wire(Valid(new DiffDeltaInfo))
  val hasDelta = VecInit(deltas.map(_.valid)).asUInt.orR
  deltaInfo.valid := hasDelta
  deltaInfo.bits.valid := hasDelta
  deltaInfo.bits.coreid := 0.U

  val nonDeltaBits = pipelined.bits.filterNot(_.bits.supportsDelta).map { b =>
    val gated = WireInit(b)
    gated.valid := b.valid && pipelined.valid
    gated.bits.bits.getValidOption.foreach(_ := b.valid && pipelined.valid)
    gated
  }

  val withDeltas = MixedVecInit((nonDeltaBits ++ deltas ++ Seq(deltaInfo)).toSeq)
  val out = IO(Decoupled(chiselTypeOf(withDeltas)))

  out.valid := VecInit(withDeltas.map(_.valid)).asUInt.orR
  out.bits := withDeltas

  pipelined.ready := !out.valid || out.ready
}
