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
  def apply(template: Seq[DifftestBundle], bundles: MixedVec[DifftestBundle], config: GatewayConfig): BatchOutput = {
    val module = Module(new BatchEndpoint(template, bundles.toSeq.map(_.cloneType), config))
    module.in := bundles
    module.out
  }

  def getTemplate(bundles: MixedVec[DifftestBundle]): Seq[DifftestBundle] = {
    val template = ListBuffer.empty[DifftestBundle]
    for (gen <- bundles) {
      if (!template.exists(_.desiredModuleName == gen.desiredModuleName)) {
        template += gen
      }
    }
    template.toSeq
  }
}

class BatchEndpoint(template: Seq[DifftestBundle], bundles: Seq[DifftestBundle], config: GatewayConfig) extends Module {
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

  def getBundleID(name: String): Int = {
    template.zipWithIndex.filter { case (gen, idx) => gen.desiredModuleName == name }.head._2
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

  // Maxixum 4000 byte packed. Now we set maxium of data byte as 3000, info as 900
  val MaxDataByteLen = 3000
  val MaxDataByteWidth = log2Ceil(MaxDataByteLen)
  val MaxDataBitLen = MaxDataByteLen * 8
  val infoWidth = (new BatchInfo).getWidth
  // Append BatchInterval and BatchFinish Info
  val MaxInfoByteLen = math.min((config.batchSize * (bundleNum + 1) + 1) * infoWidth, 900)
  val MaxInfoByteWidth = log2Ceil(MaxInfoByteLen)
  val MaxInfoBitLen = MaxInfoByteLen * 8

  val data_vec = Reg(MixedVec((1 to bundleNum).map(i => UInt(aligned_data.map(_.getWidth).take(i).sum.W))))
  val info_vec = Reg(MixedVec((1 to bundleNum).map(i => UInt((i * infoWidth).W))))
  val data_len_vec = Reg(Vec(bundleNum, UInt(MaxDataByteWidth.W)))
  val info_len_vec = Reg(Vec(bundleNum, UInt(MaxInfoByteWidth.W)))

  for (idx <- 0 until bundleNum) {
    val data_len = (aligned_data(idx).getWidth / 8).U
    val info = Wire(new BatchInfo)
    info.id := getBundleID(in(idx).desiredModuleName).U
    info.len := data_len
    if (idx == 0) {
      data_vec(idx) := Mux(delayed_valid(idx), delayed_data(idx), 0.U)
      info_vec(idx) := Mux(delayed_valid(idx), info.asUInt, 0.U)
      data_len_vec(idx) := Mux(delayed_valid(idx), data_len, 0.U)
      info_len_vec(idx) := Mux(delayed_valid(idx), (infoWidth / 8).U, 0.U)
    } else {
      data_vec(idx) := Mux(delayed_valid(idx), Cat(data_vec(idx - 1), delayed_data(idx)), data_vec(idx - 1))
      info_vec(idx) := Mux(delayed_valid(idx), Cat(info_vec(idx - 1), info.asUInt), info_vec(idx - 1))
      data_len_vec(idx) := Mux(delayed_valid(idx), data_len_vec(idx - 1) + data_len, data_len_vec(idx - 1))
      info_len_vec(idx) := Mux(delayed_valid(idx), info_len_vec(idx - 1) + (infoWidth / 8).U, info_len_vec(idx - 1))
    }
  }

  val BatchInterval = WireInit(0.U.asTypeOf(new BatchInfo))
  val BatchFinish = WireInit(0.U.asTypeOf(new BatchInfo))
  BatchInterval.id := bundleNum.U
  BatchFinish.id := (bundleNum + 1).U
  val step_data = WireInit(data_vec(bundleNum - 1))
  val step_info = Cat(info_vec(bundleNum - 1), BatchInterval.asUInt)
  val step_data_len = data_len_vec(bundleNum - 1)
  val step_info_len = info_len_vec(bundleNum - 1) + (infoWidth / 8).U

  val state_data = RegInit(0.U(MaxDataBitLen.W))
  val state_data_len = RegInit(0.U(MaxDataByteWidth.W))
  val state_info = RegInit(0.U(MaxInfoBitLen.W))
  val state_info_len = RegInit(0.U(MaxInfoByteWidth.W))
  val state_step_cnt = RegInit(0.U(log2Ceil(config.batchSize + 1).W))

  val exceed = (state_data_len +& step_data_len > MaxDataByteLen.U) | (state_info_len +& step_info_len + (infoWidth / 8).U > MaxInfoByteLen.U)
  val should_tick = delayed_enable && (exceed || state_step_cnt === config.batchSize.U)
  when(delayed_enable) {
    when(should_tick) {
      state_data := step_data
      state_data_len := step_data_len
      state_info := step_info
      state_info_len := step_info_len
      state_step_cnt := 1.U
    }.otherwise {
      state_data := state_data | step_data << (state_data_len << 3)
      state_data_len := state_data_len + step_data_len
      state_info := state_info | step_info << (state_info_len << 3)
      state_info_len := state_info_len + step_info_len
      state_step_cnt := state_step_cnt + 1.U
    }
  }

  val out = IO(Output(new BatchOutput(state_data.getWidth, state_info.getWidth, config)))
  out.data := state_data
  out.info := state_info | BatchFinish.asUInt << (state_info_len << 3)
  out.enable := should_tick
  out.step := Mux(out.enable, state_step_cnt, 0.U)
}
