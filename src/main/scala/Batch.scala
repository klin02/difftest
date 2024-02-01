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

package difftest.batch

import chisel3._
import chisel3.util._
import difftest._
import difftest.gateway.GatewayConfig
import difftest.Delayer

import scala.collection.mutable.ListBuffer

class BatchOutput(dataWidth: Int, infoWidth: Int, config: GatewayConfig) extends Bundle {
  val data = UInt(dataWidth.W)
  val info = UInt(infoWidth.W)
  val enable = Bool()
  val step = UInt(config.stepWidth.W)
}

class BatchInfo extends Bundle {
  val id = UInt(8.W)
  val len = UInt(16.W)
}

object Batch {
  def apply(bundles: MixedVec[DifftestBundle], config: GatewayConfig): BatchOutput = {
    val module = Module(new BatchEndpoint(bundles.toSeq.map(_.cloneType), config))
    module.in := bundles
    module.out
  }
}

class BatchEndpoint(bundles: Seq[DifftestBundle], config: GatewayConfig) extends Module {
  val in = IO(Input(MixedVec(bundles)))

  def bundleAlign(bundle: DifftestBundle): UInt = {
    def byteAlign(data: Data): UInt = {
      val width: Int = data.getWidth + (8 - data.getWidth % 8) % 8
      data.asTypeOf(UInt(width.W))
    }
    val element = ListBuffer.empty[UInt]
    bundle.elements.toSeq.reverse.foreach { case (name, data) =>
      if (name != "valid") {
        data match {
          case vec: Vec[_] => element ++= vec.map(byteAlign(_))
          case data: Data  => element += byteAlign(data)
        }
      }
    }
    MixedVecInit(element.toSeq).asUInt
  }

  val aligned_data = MixedVecInit(in.map(i => bundleAlign(i)).toSeq)

  val global_enable = WireInit(true.B)
  if (config.hasGlobalEnable) {
    global_enable := VecInit(in.flatMap(_.bits.needUpdate).toSeq).asUInt.orR
  }

  val bundleNum = in.length
  val delayed_enable = Delayer(global_enable, bundleNum)
  val delayed_data = MixedVecInit(aligned_data.zipWithIndex.map { case (data, i) => Delayer(data, i) }.toSeq)
  val delayed_valid = VecInit(in.zipWithIndex.map { case (gen, i) =>
    Delayer(gen.bits.getValid & global_enable, i)
  }.toSeq)

  val data_vec = Reg(MixedVec((1 to bundleNum).map(i => UInt(aligned_data.map(_.getWidth).take(i).sum.W))))
  val info_vec = Reg(MixedVec((1 to bundleNum).map(i => UInt((i * (new BatchInfo).getWidth).W))))

  for (idx <- 0 until bundleNum) {
    val info = Wire(new BatchInfo)
    info.id := idx.U
    info.len := (aligned_data(idx).getWidth / 8).U
    if (idx == 0) {
      data_vec(idx) := Mux(delayed_valid(idx), delayed_data(idx), 0.U)
      info_vec(idx) := Mux(delayed_valid(idx), info.asUInt, 0.U)
    } else {
      data_vec(idx) := Mux(delayed_valid(idx), Cat(data_vec(idx - 1), delayed_data(idx)), data_vec(idx - 1))
      info_vec(idx) := Mux(delayed_valid(idx), Cat(info_vec(idx - 1), info.asUInt), info_vec(idx - 1))
    }
  }

  val out = IO(Output(new BatchOutput(aligned_data.getWidth, bundleNum * (new BatchInfo).getWidth, config)))
  out.data := data_vec(bundleNum - 1)
  out.info := info_vec(bundleNum - 1)
  out.enable := delayed_enable
  out.step := Mux(out.enable, config.batchSize.U, 0.U)
}
