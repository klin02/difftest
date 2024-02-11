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

class BatchIO(dataType: Vec[UInt], infoType: Vec[UInt]) extends Bundle {
  val data = dataType
  val info = infoType
}

class BatchOutput(data: Vec[UInt], info: Vec[UInt], config: GatewayConfig) extends Bundle {
  val io = new BatchIO(chiselTypeOf(data), chiselTypeOf(info))
  val enable = Bool()
  val step = UInt(config.stepWidth.W)
}

class BatchInfo extends Bundle {
  val id = UInt(8.W)
}

object Batch {
  def apply(template: Seq[DifftestBundle], bundles: MixedVec[DifftestBundle], config: GatewayConfig): BatchOutput = {
    val module = Module(new BatchEndpoint(template, chiselTypeOf(bundles).toSeq, config))
    module.in := bundles
    module.out
  }

  def getTemplate(bundles: MixedVec[DifftestBundle]): Seq[DifftestBundle] = {
    chiselTypeOf(bundles).groupBy(_.desiredModuleName).values.map(_.head).toSeq
  }
}

class BatchEndpoint(template: Seq[DifftestBundle], bundles: Seq[DifftestBundle], config: GatewayConfig) extends Module {
  val in = IO(Input(MixedVec(bundles)))

  def bundleAlign(bundle: DifftestBundle): UInt = {
    def byteAlign(data: Data): UInt = {
      val width: Int = (data.getWidth + 7) / 8 * 8
      data.asTypeOf(UInt(width.W))
    }
    MixedVecInit(
      bundle.elements.toSeq.reverse
        .filterNot(bundle.isFlatten && _._1 == "valid")
        .flatMap { case (_, data) =>
          data match {
            case vec: Vec[_] => vec.map(byteAlign(_))
            case _           => Seq(byteAlign(data))
          }
        }
    ).asUInt
  }

  def getBundleID(name: String): UInt = {
    val bundleType = Seq("BatchFinish", "BatchInterval", "BatchBoot") ++
      template.map(_.desiredModuleName)
    bundleType.indexOf(name).U
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

  val MaxDataByteLen = 3900
  val MaxDataByteWidth = log2Ceil(MaxDataByteLen)
  val MaxDataBitLen = MaxDataByteLen * 8
  val infoWidth = (new BatchInfo).getWidth
  // Append BatchInterval and BatchFinish Info
  val MaxInfoByteLen = math.min((config.batchSize * (bundleNum + 1) + 1) * (infoWidth / 8), 96)
  val MaxInfoByteWidth = log2Ceil(MaxInfoByteLen)
  val MaxInfoBitLen = MaxInfoByteLen * 8

  val data_vec = Reg(MixedVec((1 to bundleNum).map(i => UInt(aligned_data.map(_.getWidth).take(i).sum.W))))
  val info_vec = Reg(MixedVec((1 to bundleNum).map(i => UInt((i * infoWidth).W))))
  val data_len_vec = Reg(Vec(bundleNum, UInt(MaxDataByteWidth.W)))
  val info_len_vec = Reg(Vec(bundleNum, UInt(MaxInfoByteWidth.W)))

  for (idx <- 0 until bundleNum) {
    val data_len = (aligned_data(idx).getWidth / 8).U
    val info = Wire(new BatchInfo)
    info.id := getBundleID(in(idx).desiredModuleName)
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

  val BatchInterval = Wire(new BatchInfo)
  BatchInterval.id := getBundleID("BatchInterval")
  val step_data = WireInit(data_vec(bundleNum - 1))
  val step_info = Cat(info_vec(bundleNum - 1), BatchInterval.asUInt)
  val step_data_len = data_len_vec(bundleNum - 1)
  val step_info_len = info_len_vec(bundleNum - 1) + (BatchInterval.getWidth / 8).U

  val BatchBank = 4
  val state_data = RegInit(0.U.asTypeOf(Vec(BatchBank, UInt(MaxDataBitLen.W))))
  val state_data_len = RegInit(0.U(MaxDataByteWidth.W))
  val state_info = RegInit(0.U.asTypeOf(Vec(BatchBank, UInt(MaxInfoBitLen.W))))
  val state_info_len = RegInit(0.U(MaxInfoByteWidth.W))
  val state_bank_cnt = RegInit(0.U(log2Ceil(BatchBank).W))
  val state_step_cnt = RegInit(0.U(log2Ceil(config.batchSize + 1).W))

  val exceed = (state_data_len +& step_data_len > MaxDataByteLen.U) |
    (state_info_len +& step_info_len > (MaxInfoByteLen - infoWidth / 8).U)
  val should_tick =
    delayed_enable && ((exceed && state_bank_cnt === (BatchBank - 1).U) || state_step_cnt === config.batchSize.U)
  val BatchBoot = Wire(new BatchInfo)
  BatchBoot.id := getBundleID("BatchBoot")
  val BootIndex = WireInit(0.U(infoWidth.W))
  BootIndex := Mux(should_tick, 0.U, state_step_cnt)
  val BatchHead = Cat(BootIndex, BatchBoot.asUInt)
  val state_next_bank = Mux(should_tick, 0.U, state_bank_cnt + 1.U)
  when(delayed_enable) {
    when(should_tick || exceed) {
      state_data(state_next_bank) := step_data
      state_data_len := step_data_len
      state_info(state_next_bank) := Cat(step_info, BatchHead)
      state_info_len := step_info_len + (BatchHead.getWidth / 8).U
      state_bank_cnt := state_next_bank
    }.otherwise {
      state_data(state_bank_cnt) := state_data(state_bank_cnt) | step_data << (state_data_len << 3)
      state_data_len := state_data_len + step_data_len
      state_info(state_bank_cnt) := state_info(state_bank_cnt) | step_info << (state_info_len << 3)
      state_info_len := state_info_len + step_info_len
    }

    when(should_tick) {
      state_step_cnt := 1.U
    }.otherwise {
      state_step_cnt := state_step_cnt + 1.U
    }
  }

  val out = IO(Output(new BatchOutput(state_data, state_info, config)))
  out.io.data := state_data
  out.io.info := state_info
  out.enable := should_tick
  out.step := Mux(out.enable, state_step_cnt, 0.U)
}
