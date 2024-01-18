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

object Batch {
  def apply[T <: Seq[DifftestBundle]](bundles: T, config: GatewayConfig): BatchEndpoint = {
    val module = Module(new BatchEndpoint(bundles, config))
    module
  }
}

class BatchEndpoint(bundles: Seq[DifftestBundle], config: GatewayConfig) extends Module {
  val in = IO(Input(MixedVec(bundles)))
  val data = IO(Output(UInt(in.getWidth.W)))
  val lenVec = VecInit(in.map(i => Mux(i.bits.getValid, i.asUInt.getWidth.U(16.W), 0.U)).toSeq)
  val sumVec = VecInit((1 to in.length).map(i => lenVec.take(i).reduce(_ + _)))
  val dataVec = VecInit(Seq.fill(in.length)(0.U.asTypeOf(in.asUInt)))
  for ((gen, idx) <- in.zipWithIndex) {
    val offset = if(idx != 0) sumVec(idx - 1) else 0.U
    dataVec(idx) := gen.asUInt << offset
  }
  data := dataVec.reduce(_ | _)
  dontTouch(data)

}