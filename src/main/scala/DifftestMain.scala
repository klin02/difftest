/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
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

package difftest

import circt.stage.{ChiselStage, FirtoolOption}
import chisel3._
import chisel3.util._
import chisel3.stage._

// Main class to generat difftest modules when design is not written in chisel.
class DifftestTop extends Module {
  def Module(bc: => DifftestModule) = chisel3.Module(bc).io <> DontCare
  val difftest_arch_event = Module(new DifftestArchEvent);
  val difftest_basic_instr_commit = Module(new DifftestBasicInstrCommit);
  val difftest_instr_commit = Module(new DifftestInstrCommit);
  val difftest_basic_trap_event = Module(new DifftestBasicTrapEvent);
  val difftest_trap_event = Module(new DifftestTrapEvent);
  val difftest_csr_state = Module(new DifftestCSRState);
  val difftest_debug_mode = Module(new DifftestDebugMode);
  val difftest_int_writeback = Module(new DifftestIntWriteback);
  val difftest_fp_writeback = Module(new DifftestFpWriteback);
  val difftest_arch_int_reg_state = Module(new DifftestArchIntRegState);
  val difftest_arch_fp_reg_state = Module(new DifftestArchFpRegState);
  val difftest_sbuffer_event = Module(new DifftestSbufferEvent);
  val difftest_store_event = Module(new DifftestStoreEvent);
  val difftest_load_event = Module(new DifftestLoadEvent);
  val difftest_atomic_event = Module(new DifftestAtomicEvent);
  val difftest_itlb_event = Module(new DifftestL1TLBEvent);
  val difftest_ldtlb_event = Module(new DifftestL1TLBEvent);
  val difftest_sttlb_event = Module(new DifftestL1TLBEvent);
  val difftest_l2tlb_event = Module(new DifftestL2TLBEvent);
  val difftest_irefill_event = Module(new DifftestRefillEvent);
  val difftest_drefill_event = Module(new DifftestRefillEvent);
  val difftest_ptwrefill_event = Module(new DifftestRefillEvent);
  val difftest_lr_sc_event = Module(new DifftestLrScEvent);
  val difftest_runahead_event = Module(new DifftestRunaheadEvent);
  val difftest_runahead_commit_event = Module(new DifftestRunaheadCommitEvent);
  val difftest_runahead_redirect_event = Module(new DifftestRunaheadRedirectEvent);
  val difftest_runahead_memdep_pred = Module(new DifftestRunaheadMemdepPred);

}

object DifftestMain extends App {
  (new ChiselStage).execute(Array("--target", "verilog") ++ args, Seq(
    ChiselGeneratorAnnotation(() => new DifftestTop)
  ))
}
