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
import difftest.common.DifftestPerf
import difftest.util.Delayer

import scala.collection.mutable.ListBuffer

case class BatchParam(config: GatewayConfig, templateLen: Int) {
  val infoWidth = (new BatchInfo).getWidth

  val MaxDataByteLen = config.batchArgByteLen._1
  val MaxDataByteWidth = log2Ceil(MaxDataByteLen)
  val MaxDataBitLen = MaxDataByteLen * 8

  val MaxInfoByteLen = config.batchArgByteLen._2
  val MaxInfoByteWidth = log2Ceil(MaxInfoByteLen)
  val MaxInfoBitLen = MaxInfoByteLen * 8
}

class BatchIO(dataType: UInt, infoType: UInt) extends Bundle {
  val data = dataType
  val info = infoType
}

class BatchOutput(data: UInt, info: UInt, config: GatewayConfig) extends Bundle {
  val io = new BatchIO(chiselTypeOf(data), chiselTypeOf(info))
  val enable = Bool()
  val step = UInt(config.stepWidth.W)
}

class BatchInfo extends Bundle {
  val id = UInt(8.W)
  val num = UInt(8.W)
}

object Batch {
  private val template = ListBuffer.empty[DifftestBundle]

  def apply(bundles: MixedVec[Valid[DifftestBundle]], config: GatewayConfig): BatchOutput = {
    template ++= chiselTypeOf(bundles).map(_.bits).distinctBy(_.desiredCppName)
    val param = BatchParam(config, template.length)
    val module = Module(new BatchEndpoint(chiselTypeOf(bundles).toSeq, config, param))
    module.in := bundles
    module.out
  }

  def getTemplate: Seq[DifftestBundle] = template.toSeq

  def getBundleID(bundleType: DifftestBundle): Int = {
    template.indexWhere(_.desiredCppName == bundleType.desiredCppName)
  }
}

class BatchEndpoint(bundles: Seq[Valid[DifftestBundle]], config: GatewayConfig, param: BatchParam) extends Module {
  val in = IO(Input(MixedVec(bundles)))
  def vecAlignWidth = (vec: Seq[Valid[DifftestBundle]]) => vec.head.bits.getByteAlign.getWidth * vec.length

  // Collect bundles with valid in Pipeline
  val global_enable = VecInit(in.map(_.valid).toSeq).asUInt.orR
  val inCollect =
    in.groupBy(_.bits.desiredCppName).values.toSeq.map(_.toSeq).sortBy(vecAlignWidth).reverse
  val inCollect_w = inCollect.map(vecAlignWidth)
  val dataCollect_vec = WireInit(
    0.U.asTypeOf(
      MixedVec(
        Seq.tabulate(inCollect.length)(i => UInt(inCollect_w.take(i + 1).sum.W))
      )
    )
  )
  val infoCollect_vec = WireInit(
    0.U.asTypeOf(
      MixedVec(
        Seq.tabulate(inCollect.length)(i => UInt(((i + 1) * param.infoWidth).W))
      )
    )
  )
  val dataLenCollect_vec = WireInit(0.U.asTypeOf(Vec(inCollect.length, UInt(param.MaxDataByteWidth.W))))
  val infoLenCollect_vec = WireInit(0.U.asTypeOf(Vec(inCollect.length, UInt(param.MaxInfoByteWidth.W))))
  inCollect.zipWithIndex.foreach { case (in, idx) =>
    val (dataBaseW, infoBaseW) = if (idx != 0) {
      (dataCollect_vec(idx - 1).getWidth, infoCollect_vec(idx - 1).getWidth)
    } else {
      (0, 0)
    }
    val collector = Module(
      new BatchCollector(
        chiselTypeOf(in.head),
        in.length,
        dataBaseW,
        infoBaseW,
        param,
        idx,
      )
    )
    collector.data_in := in
    collector.enable := global_enable
    if (idx != 0) {
      collector.data_base := dataCollect_vec(idx - 1)
      collector.info_base := infoCollect_vec(idx - 1)
      collector.data_len_base := dataLenCollect_vec(idx - 1)
      collector.info_len_base := infoLenCollect_vec(idx - 1)
    } else {
      collector.data_base := 0.U
      collector.info_base := 0.U
      collector.data_len_base := 0.U
      collector.info_len_base := 0.U
    }
    dataCollect_vec(idx) := collector.data_out
    infoCollect_vec(idx) := collector.info_out
    dataLenCollect_vec(idx) := collector.data_len_out
    infoLenCollect_vec(idx) := collector.info_len_out
  }

  val BatchInterval = WireInit(0.U.asTypeOf(new BatchInfo))
  val BatchFinish = WireInit(0.U.asTypeOf(new BatchInfo))
  BatchInterval.id := Batch.getTemplate.length.U
  BatchFinish.id := (Batch.getTemplate.length + 1).U
  val step_data = dataCollect_vec.last
  val step_info = Cat(infoCollect_vec.last, BatchInterval.asUInt)
  val step_data_len = dataLenCollect_vec.last
  val step_info_len = infoLenCollect_vec.last + (param.infoWidth / 8).U
  assert(step_data_len <= param.MaxDataByteLen.U)
  assert(step_info_len <= param.MaxInfoByteLen.U)

  val state_data = RegInit(0.U(param.MaxDataBitLen.W))
  val state_data_len = RegInit(0.U(param.MaxDataByteWidth.W))
  val state_info = RegInit(0.U(param.MaxInfoBitLen.W))
  val state_info_len = RegInit(0.U(param.MaxInfoByteWidth.W))
  val state_step_cnt = RegInit(0.U(config.stepWidth.W))
  val state_trace_size = Option.when(config.hasReplay)(RegInit(0.U(16.W)))

  val delayed_enable = Delayer(global_enable, inCollect.length)

  val (delayed_trace_size, delayed_in_replay) = if (config.hasReplay) {
    val trace_info = in.map(_.bits).filter(_.desiredCppName == "trace_info").head.asInstanceOf[DiffTraceInfo]
    (Some(Delayer(trace_info.trace_size, inCollect.length)), Some(Delayer(trace_info.in_replay, inCollect.length)))
  } else (None, None)
  val data_exceed = delayed_enable && (state_data_len +& step_data_len > param.MaxDataByteLen.U)
  val info_exceed =
    delayed_enable && (state_info_len +& step_info_len + (param.infoWidth / 8).U > param.MaxInfoByteLen.U)
  val step_exceed = delayed_enable && (state_step_cnt === config.batchSize.U)
  val trace_exceed = Option.when(config.hasReplay) {
    delayed_enable && (state_trace_size.get +& delayed_trace_size.get +& inCollect.length.U >= config.replaySize.U)
  }
  if (config.hasBuiltInPerf) {
    DifftestPerf("BatchExceed_data", data_exceed.asUInt)
    DifftestPerf("BatchExceed_info", info_exceed.asUInt)
    DifftestPerf("BatchExceed_step", step_exceed.asUInt)
    if (config.hasReplay) DifftestPerf("BatchExceed_trace", trace_exceed.get.asUInt)
  }

  val should_tick =
    data_exceed || info_exceed || step_exceed || trace_exceed.getOrElse(false.B) || delayed_in_replay.getOrElse(false.B)
  when(delayed_enable) {
    when(should_tick) {
      state_data := step_data
      state_data_len := step_data_len
      state_info := step_info
      state_info_len := step_info_len
      state_step_cnt := 1.U
      if (config.hasReplay) state_trace_size.get := delayed_trace_size.get
    }.otherwise {
      state_data := state_data | step_data << (state_data_len << 3)
      state_data_len := state_data_len + step_data_len
      state_info := state_info | step_info << (state_info_len << 3)
      state_info_len := state_info_len + step_info_len
      state_step_cnt := state_step_cnt + 1.U
      if (config.hasReplay) state_trace_size.get := state_trace_size.get + delayed_trace_size.get
    }
  }

  val out = IO(Output(new BatchOutput(state_data, state_info, config)))
  out.io.data := state_data
  out.io.info := state_info | BatchFinish.asUInt << (state_info_len << 3)
  out.enable := should_tick
  out.step := Mux(out.enable, state_step_cnt, 0.U)
}

// Collect Bundles with Valid by pipeline, same Class will be processed in parallel
class BatchCollector(
  bundleType: Valid[DifftestBundle],
  length: Int,
  dataBase_w: Int,
  infoBase_w: Int,
  param: BatchParam,
  delay: Int,
) extends Module {
  val data_in = IO(Input(Vec(length, bundleType)))
  val enable = IO(Input(Bool()))

  val alignWidth = data_in.head.bits.getByteAlign.getWidth
  val dataOut_w = dataBase_w + alignWidth * length
  val infoOut_w = infoBase_w + param.infoWidth

  val data_base = IO(Input(UInt(dataBase_w.W)))
  val info_base = IO(Input(UInt(infoBase_w.W)))
  val data_len_base = IO(Input(UInt(param.MaxDataByteWidth.W)))
  val info_len_base = IO(Input(UInt(param.MaxInfoByteWidth.W)))

  val data_out = IO(Output(UInt(dataOut_w.W)))
  val info_out = IO(Output(UInt(infoOut_w.W)))
  val data_len_out = IO(Output(UInt(param.MaxDataByteWidth.W)))
  val info_len_out = IO(Output(UInt(param.MaxInfoByteWidth.W)))

  val data_state = RegInit(0.U(dataOut_w.W))
  val info_state = RegInit(0.U(infoOut_w.W))
  val data_len_state = RegInit(0.U(param.MaxDataByteWidth.W))
  val info_len_state = RegInit(0.U(param.MaxInfoByteWidth.W))

  val align_data = VecInit(data_in.map(i => i.bits.getByteAlign).toSeq)
  val valid_vec = VecInit(data_in.map(i => i.valid && enable))
  val delay_data = Delayer(align_data.asUInt, delay, useMem = true).asTypeOf(align_data)
  val delay_valid = Delayer(valid_vec.asUInt, delay, useMem = true).asTypeOf(valid_vec)

  val valid_num = PopCount(delay_valid)
  val info = Wire(new BatchInfo)
  info.id := Batch.getBundleID(bundleType.bits).U
  info.num := valid_num

  val offset_map = (0 to length).map(i => i.U -> (i * alignWidth).U)
  val dataLen_map = (0 to length).map(i => i.U -> (i * alignWidth / 8).U)

  val data_site = WireInit(0.U((alignWidth * length).W))
  data_site := VecInit(delay_data.zipWithIndex.map { case (d, idx) =>
    val offset = if (idx == 0) 0.U else MuxLookup(PopCount(delay_valid.take(idx)), 0.U)(offset_map)
    Mux(delay_valid(idx), (d << offset).asUInt, 0.U)
  }.toSeq).reduce(_ | _)

  when(delay_valid.asUInt.orR) {
    data_state := (data_base << MuxLookup(valid_num, 0.U)(offset_map)).asUInt | data_site
    info_state := Cat(info_base, info.asUInt)
    data_len_state := data_len_base + MuxLookup(valid_num, 0.U)(dataLen_map)
    info_len_state := info_len_base + (param.infoWidth / 8).U
  }.otherwise {
    data_state := data_base
    info_state := info_base
    data_len_state := data_len_base
    info_len_state := info_len_base
  }

  data_out := data_state
  info_out := info_state
  data_len_out := data_len_state
  info_len_out := info_len_state
}
