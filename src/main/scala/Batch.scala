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

object Batch {
  def apply[T <: Seq[DifftestBundle]](bundles: T, config: GatewayConfig): BatchEndpoint = {
    val module = Module(new BatchEndpoint(bundles, config))
    module
  }

}

class BatchEndpoint(bundles: Seq[DifftestBundle], config: GatewayConfig) extends Module {
  val in = IO(Input(MixedVec(bundles)))

  def byteAlign(data: Data): UInt = {
    val width: Int = data.getWidth + (8 - data.getWidth % 8) % 8
    data.asTypeOf(UInt(width.W))
  }

  val aligned = MixedVecInit(in.map{ i =>
    val element = ListBuffer.empty[UInt]
    i.elements.toSeq.reverse.foreach { case (_, data) =>
      data match {
        case vec: Vec[_] => element ++= vec.map(byteAlign(_))
        case data: Data => element += byteAlign(data)
      }
    }
    MixedVecInit(element.toSeq).asUInt
  }.toSeq)

  dontTouch(aligned)
  val global_enable = WireInit(true.B)
  if (config.hasGlobalEnable) {
    global_enable := VecInit(in.flatMap(_.bits.needUpdate).toSeq).asUInt.orR
  }
  val validVec = VecInit(in.map(_.bits.getValid & global_enable).toSeq)
  //byteLen
  val lenVec = VecInit(aligned.zip(validVec).map{case (a,v) => Mux(v, (a.getWidth >> 3).U(16.W), 0.U)}.toSeq)
  val sumVec = VecInit((1 to aligned.length).map(i => lenVec.take(i).reduce(_ + _)))
  val dataVec = VecInit(Seq.fill(aligned.length)(0.U.asTypeOf(aligned.asUInt)))
  val infoVec = VecInit(Seq.fill(aligned.length)(0.U.asTypeOf(new BatchInfoBundle)))

  for ((alignGen, idx) <- aligned.zipWithIndex) {
    val byteOffset = if(idx != 0) sumVec(idx - 1) else 0.U
    dataVec(idx) := alignGen.asUInt << (byteOffset << 3)
    infoVec(idx).id := idx.U
    infoVec(idx).offset := byteOffset
  }

  val out = IO(Output(UInt(aligned.getWidth.W)))
  val info = IO(Output(UInt(infoVec.getWidth.W)))
  val enable = IO(Output(Bool()))
  out := dataVec.reduce(_ | _)
  info := infoVec.asUInt
  enable := global_enable
  dontTouch(out)
  dontTouch(info)
}

class BatchInfoBundle extends Bundle {
  val id = UInt(8.W)
  val offset = UInt(16.W)
}