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

case class BatchParam(config: GatewayConfig, dataWidth: Int) {
  val infoWidth = (new BatchInfo).getWidth

  val MaxDataByteLen = config.batchArgByteLen._1
  val MaxDataBitLen = MaxDataByteLen * 8

  val MaxInfoByteLen = config.batchArgByteLen._2
  val MaxInfoBitLen = MaxInfoByteLen * 8

  val BitLenWidth = math.max(log2Ceil(MaxDataBitLen), log2Ceil(dataWidth))
  val ByteLenWidth = BitLenWidth - 3
}

class BatchIO(dataType: UInt, infoType: UInt) extends Bundle {
  val data = dataType
  val info = infoType
}

class BatchStats(ByteLenWidth: Int) extends Bundle {
  val data_len = UInt(ByteLenWidth.W)
  val info_len = UInt(ByteLenWidth.W)
}

class BatchOutput(dataType: UInt, infoType: UInt, config: GatewayConfig) extends Bundle {
  val io = new BatchIO(dataType, infoType)
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
    val module = Module(new BatchEndpoint(chiselTypeOf(bundles).toSeq, config))
    module.in := bundles
    module.out
  }

  def getTemplate: Seq[DifftestBundle] = template.toSeq

  def getBundleID(bundleType: DifftestBundle): Int = {
    template.indexWhere(_.desiredCppName == bundleType.desiredCppName)
  }
}

class BatchEndpoint(bundles: Seq[Valid[DifftestBundle]], config: GatewayConfig) extends Module {
  val in = IO(Input(MixedVec(bundles)))
  def vecAlignWidth = (vec: Seq[Valid[DifftestBundle]]) => vec.head.bits.getByteAlign.getWidth * vec.length

  // Collect bundles with valid of same cycle in Pipeline
  val global_enable = VecInit(in.map(_.valid).toSeq).asUInt.orR
  val inCollect =
    in.groupBy(_.bits.desiredCppName).values.toSeq.map(_.toSeq).sortBy(vecAlignWidth).reverse
  val inCollect_w = inCollect.map(vecAlignWidth)
  val param = BatchParam(config, inCollect_w.sum)
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
  val statsCollect_vec = WireInit(0.U.asTypeOf(Vec(inCollect.length, new BatchStats(param.ByteLenWidth))))
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
      collector.stats_base := statsCollect_vec(idx - 1)
    } else {
      collector.data_base := 0.U
      collector.info_base := 0.U
      collector.stats_base := 0.U.asTypeOf(new BatchStats(param.ByteLenWidth))
    }
    dataCollect_vec(idx) := collector.data_out
    infoCollect_vec(idx) := collector.info_out
    statsCollect_vec(idx) := collector.stats_out
  }

  val BatchInterval = WireInit(0.U.asTypeOf(new BatchInfo))
  BatchInterval.id := Batch.getTemplate.length.U
  val step_data = dataCollect_vec.last
  val step_info = infoCollect_vec.last
//  val step_info = Cat(infoCollect_vec.last, BatchInterval.asUInt)
//  val step_info = infoCollect_vec.last | BatchInterval.asUInt << (statsCollect_vec.last.info_len << 3)
  val step_stats_vec = {
    val collected = statsCollect_vec.zipWithIndex.map{ case (stats, idx) =>
      Delayer(stats, inCollect.length - idx - 1)
    }
    val appended = VecInit(collected)
//    appended.last.info_len := collected.last.info_len + (param.infoWidth / 8).U
    appended
  }

  // Assemble collected data from different cycles
  val assembler = Module(new BatchAssembler(step_data.getWidth, step_info.getWidth, inCollect.length, param, config))
  assembler.step_data := step_data
  assembler.step_info := step_info
  assembler.step_stats_vec := step_stats_vec
//  assembler.step_stats.data_len := statsCollect_vec.last.data_len
//  assembler.step_stats.info_len := statsCollect_vec.last.info_len + (param.infoWidth / 8).U
//  assert(step_data_len <= param.MaxDataByteLen.U)
//  assert(step_info_len <= param.MaxInfoByteLen.U)

  assembler.enable := Delayer(global_enable, inCollect.length)
  if (config.hasReplay) {
    val trace_info = in.map(_.bits).filter(_.desiredCppName == "trace_info").head.asInstanceOf[DiffTraceInfo]
    assembler.step_trace_info.get := Delayer(trace_info, inCollect.length)
  }

  val assembled = WireInit(assembler.out)
  val out = IO(Output(chiselTypeOf(assembled)))
  out := assembled
//  val out = WireInit(assembler.out)
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
  val alignWidth = bundleType.bits.getByteAlignWidth
  val dataOut_w = dataBase_w + alignWidth * length
  val infoOut_w = infoBase_w + param.infoWidth

  val data_in = IO(Input(Vec(length, bundleType)))
  val enable = IO(Input(Bool()))

  val data_base = IO(Input(UInt(dataBase_w.W)))
  val info_base = IO(Input(UInt(infoBase_w.W)))
  val stats_base = IO(Input(new BatchStats(param.ByteLenWidth)))

  val data_out = IO(Output(UInt(dataOut_w.W)))
  val info_out = IO(Output(UInt(infoOut_w.W)))
  val stats_out = IO(Output(new BatchStats(param.ByteLenWidth)))

  val data_state = RegInit(0.U(dataOut_w.W))
  val info_state = RegInit(0.U(infoOut_w.W))
  val stats_state = RegInit(0.U.asTypeOf(new BatchStats(param.ByteLenWidth)))

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
//    data_state := data_base | (data_site << (stats_base.data_len << 3))
//    info_state := info_base | (info.asUInt << (stats_base.info_len << 3))
    stats_state.data_len := stats_base.data_len + MuxLookup(valid_num, 0.U)(dataLen_map)
    stats_state.info_len := stats_base.info_len + (param.infoWidth / 8).U
  }.otherwise {
    data_state := data_base
    info_state := info_base
    stats_state := stats_base
  }

  data_out := data_state
  info_out := info_state
  stats_out := stats_state
}

class BatchAssembler(
  step_data_w: Int,
  step_info_w: Int,
  collect_length: Int,
  param: BatchParam,
  config: GatewayConfig,
) extends Module {
  val enable = IO(Input(Bool()))
  val step_data = IO(Input(UInt(step_data_w.W)))
  val step_info = IO(Input(UInt(step_info_w.W)))
  val step_stats_vec = IO(Input(Vec(collect_length, new BatchStats(param.ByteLenWidth))))
  val step_trace_info = Option.when(config.hasReplay)(IO(Input(new DiffTraceInfo(config))))

  val state_data = RegInit(0.U(param.MaxDataBitLen.W))
  val state_info = RegInit(0.U(param.MaxInfoBitLen.W))
  val state_stats = RegInit(0.U.asTypeOf(new BatchStats(param.ByteLenWidth)))
  val state_step_cnt = RegInit(0.U(config.stepWidth.W))
  val state_trace_size = Option.when(config.hasReplay)(RegInit(0.U(param.ByteLenWidth.W)))

  val data_limit = param.MaxDataByteLen.U -& state_stats.data_len
  val info_limit = (param.MaxInfoByteLen - 2 * param.infoWidth / 8).U -& state_stats.info_len
  val data_exceed_vec = VecInit(step_stats_vec.map(_.data_len > data_limit && enable))
  val info_exceed_vec = VecInit(step_stats_vec.map(_.info_len > info_limit && enable))

//  val data_exceed = enable && (state_stats.data_len +& step_stats.data_len > param.MaxDataByteLen.U)
//  val info_exceed =
//    enable && (state_stats.info_len +& step_stats.info_len + (param.infoWidth / 8).U > param.MaxInfoByteLen.U)
  val step_exceed = enable && (state_step_cnt === config.batchSize.U)
  val trace_exceed = Option.when(config.hasReplay) {
    enable && (state_trace_size.get +& step_trace_info.get.trace_size +& collect_length.U >= config.replaySize.U)
  }
  if (config.hasBuiltInPerf) {
    DifftestPerf("BatchExceed_data", data_exceed_vec.asUInt.orR)
    DifftestPerf("BatchExceed_info", info_exceed_vec.asUInt.orR)
    DifftestPerf("BatchExceed_step", step_exceed.asUInt)
    if (config.hasReplay) DifftestPerf("BatchExceed_trace", trace_exceed.get.asUInt)
  }

  val in_replay = Option.when(config.hasReplay)(step_trace_info.get.in_replay)
  val should_tick =
    data_exceed_vec.asUInt.orR || info_exceed_vec.asUInt.orR || step_exceed || trace_exceed.getOrElse(false.B) || in_replay.getOrElse(false.B)

  val exceed_vec = VecInit(data_exceed_vec.zip(info_exceed_vec).map{ case (de, ie) => de | ie})
  // extract last non-exceed stats
  val concat_stats = VecInit(step_stats_vec.dropRight(1).zipWithIndex.map { case (stats, idx) =>
      val mask = exceed_vec(idx) ^ exceed_vec(idx + 1)
      Mux(mask, stats.asUInt, 0.U)
  }).reduceTree(_ | _).asTypeOf(new BatchStats(param.ByteLenWidth))

  val remain_stats = WireInit(0.U.asTypeOf(new BatchStats(param.ByteLenWidth)))
  remain_stats.data_len := step_stats_vec.last.data_len -& concat_stats.data_len
  remain_stats.info_len := step_stats_vec.last.info_len -& concat_stats.info_len

//  val concat_data = ((1.U << (concat_stats.data_len << 3).asUInt).asUInt - 1.U) & step_data
//  val concat_info = ((1.U << ((concat_stats.info_len + 1.U) << 3).asUInt).asUInt - 1.U) & step_info
//  val remain_data = step_data >> (concat_stats.data_len << 3)
//  val remain_info = step_info >> ((concat_stats.info_len + 1.U) << 3)

  val concat_data = step_data >> (remain_stats.data_len << 3)
  val concat_info = step_info >> (remain_stats.info_len << 3)
  val remain_data = ((1.U << (remain_stats.data_len << 3).asUInt).asUInt - 1.U) & step_data
  val remain_info = ((1.U << (remain_stats.info_len << 3).asUInt).asUInt - 1.U) & step_info
//  val info = step_info.asTypeOf(Vec(collect_length, new BatchInfo))
//  when(exceed_vec.asUInt.andR) {
//    printf("%x\n", step_data)
//    printf("%x\n", concat_data)
//    printf("%x\n", remain_data(3999,0))
//    printf("%x\n", step_info)
//    printf("%x\n", concat_info)
//    printf("%x\n", remain_info(1000,0))
//  }
////    printf(p"info: ${exceed_vec.asUInt}\n")
////    printf(p"${step_stats_vec.last.data_len}\n")
////    printf(p"${split_stats.data_len}\n")
////    printf("%d\n", step_stats_vec.last.data_len)
////    printf("%d\n", split_stats.data_len)
////    step_stats_vec.foreach(s => printf("%d ", s.data_len))
////    printf(p"${remain_data(231, 224)}\n")
////    printf(p"${remain_data(487, 480)}\n")
////    printf(p"${remain_data(1647, 1640)}\n")
////    printf(p"${step_data(2287, 2280)}\n")
////    printf(p"${step_data(2543, 2536)}\n")
////    printf(p"${step_data(3703, 3696)}\n")
//////    info.foreach(i => printf(p"${i.id} "))
////    printf("%x\n", step_data)
//////    printf("%x\n", concat_data(8000, 0))
//////    printf("%x\n", remain_data)
////
//    printf("%x\n", step_info(3999, 0))
//    printf("%x\n", concat_info(3999, 0))
//    printf("%x\n", remain_info(3999, 0))
//  }

  val BatchInterval = WireInit(0.U.asTypeOf(new BatchInfo))
  BatchInterval.id := Batch.getTemplate.length.U

  val has_concat = exceed_vec.asUInt.orR && !exceed_vec.asUInt.andR
  when(enable) {
    when(should_tick) {
//      state_data := step_data
//      state_info := step_info
//      state_stats := step_stats_vec.last
      state_data := remain_data
      when(has_concat) {
        state_info := remain_info
        state_stats := remain_stats
      }.otherwise {
        state_info := Cat(remain_info, BatchInterval.asUInt)
        state_stats.data_len := remain_stats.data_len
        state_stats.info_len := remain_stats.info_len + (param.infoWidth / 8).U
      }

      state_step_cnt := 1.U
      if (config.hasReplay) state_trace_size.get := step_trace_info.get.trace_size
    }.otherwise {
      state_data := state_data | step_data << (state_stats.data_len << 3)
      state_info := state_info | Cat(step_info, BatchInterval.asUInt) << (state_stats.info_len << 3)
      state_stats.data_len := state_stats.data_len + step_stats_vec.last.data_len
      state_stats.info_len := state_stats.info_len + step_stats_vec.last.info_len + (param.infoWidth / 8).U
      state_step_cnt := state_step_cnt + 1.U
      if (config.hasReplay) state_trace_size.get := state_trace_size.get + step_trace_info.get.trace_size
    }
  }

  val BatchFinish = WireInit(0.U.asTypeOf(new BatchInfo))
  BatchFinish.id := (Batch.getTemplate.length + 1).U
  BatchFinish.num := state_step_cnt

  val out = IO(Output(new BatchOutput(chiselTypeOf(state_data), chiselTypeOf(state_info), config)))
//  out.io.data := state_data
//  out.io.info := state_info | BatchFinish.asUInt << (state_stats.info_len << 3)
  out.io.data := state_data | Mux(has_concat, concat_data << (state_stats.data_len << 3), 0.U)
  val fin_offset = state_stats.info_len + Mux(has_concat, concat_stats.info_len + (param.infoWidth / 8).U, 0.U)
  val append_info = Mux(has_concat,
    Cat(concat_info, BatchInterval.asUInt) | BatchFinish.asUInt << ((concat_stats.info_len + (param.infoWidth / 8).U) << 3),
    BatchFinish.asUInt
  )
//  out.io.info := state_info | Mux(has_concat, Cat(concat_info, BatchInterval.asUInt) << (state_stats.info_len << 3), 0.U) | BatchFinish.asUInt << (fin_offset << 3)
  out.io.info := state_info | append_info << (state_stats.info_len << 3)
  out.enable := should_tick
  out.step := Mux(out.enable, state_step_cnt, 0.U)
}
