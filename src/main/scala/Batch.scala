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

import scala.collection.mutable.ListBuffer

class BatchOutput(dataWidth: Int, infoWidth: Int, config: GatewayConfig) extends Bundle {
  val data = UInt(dataWidth.W)
  val info = UInt(infoWidth.W)
  val enable = Bool()
  val step = UInt(config.stepWidth.W)
}

class BatchInfo extends Bundle {
  val id = UInt(8.W)
  val offset = UInt(16.W)
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

  val alignedData = MixedVecInit(in.map(i => bundleAlign(i)).toSeq)

  val need_store = WireInit(true.B)
  if (config.hasGlobalEnable) {
    need_store := VecInit(in.flatMap(_.bits.needUpdate).toSeq).asUInt.orR
  }
  val bundleNum = in.length
  val validVec = VecInit(in.map(_.bits.getValid & need_store).toSeq)
  val dataLenVec = VecInit(alignedData.zip(validVec).map { case (a, v) => Mux(v, a.getWidth.U(15.W), 0.U) }.toSeq)
  val dataSumVec = VecInit((1 to bundleNum).map(i => dataLenVec.take(i).reduce(_ + _)))
  val infoWidth = (new BatchInfo).getWidth
  val infoLenVec = validVec.map { v => Mux(v, infoWidth.U(15.W), 0.U) }
  val infoSumVec = VecInit((1 to bundleNum).map(i => infoLenVec.take(i).reduce(_ + _)))
  val dataVec = VecInit(Seq.fill(bundleNum)(0.U.asTypeOf(alignedData.asUInt)))
  val infoVec = VecInit(Seq.fill(bundleNum)(0.U((bundleNum * infoWidth).W)))

  for ((alignGen, idx) <- alignedData.zipWithIndex) {
    val dataBitOffset = if (idx != 0) dataSumVec(idx - 1) else 0.U
    val infoBitOffset = if (idx != 0) infoSumVec(idx - 1) else 0.U
    dataVec(idx) := Mux(validVec(idx), alignGen << dataBitOffset, 0.U)
    val info = Wire(new BatchInfo)
    info.id := idx.U
    info.offset := dataBitOffset >> 3
    infoVec(idx) := Mux(validVec(idx), info.asUInt << infoBitOffset, 0.U)
  }
  val out = IO(Output(new BatchOutput(alignedData.getWidth, bundleNum * infoWidth, config)))
  out.data := dataVec.reduce(_ | _)
  out.info := infoVec.reduce(_ | _)
  out.enable := need_store
  out.step := Mux(out.enable, config.batchSize.U, 0.U)
}
