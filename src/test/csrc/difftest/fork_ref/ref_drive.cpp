/***************************************************************************************
* Copyright (c) 2025-2026 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
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
#include "ref_drive.h"
#include "flash.h"
#include "ram.h"

void ref_drive_init(RefProxy &proxy, const DiffTestRegState &regs) {
  proxy.flash_init((const uint8_t *)flash_dev.base, flash_dev.img_size, flash_dev.img_path);
  simMemory->clone_on_demand(
      [&proxy](uint64_t offset, void *src, size_t n) { proxy.mem_init(PMEM_BASE + offset, src, n, DUT_TO_REF); }, true);
  proxy.regcpy(&regs, FIRST_INST_ADDRESS);
}

#ifdef CONFIG_DIFFTEST_FAST_REF
#include "diffstate.h"
#include <algorithm>

bool RefDriveExecutor::execute(uint64_t instr_count) {
  while (instr_count) {
    const uint64_t batch = std::min(instr_count, MAX_PENDING - pending_instrs);
    pending_instrs += batch;
    instr_count -= batch;
    if (pending_instrs == MAX_PENDING && !flush())
      return false;
  }
  return true;
}

bool RefDriveExecutor::flush() {
  if (!pending_instrs)
    return true;
  const uint64_t requested = pending_instrs;
  pending_instrs = 0;
  const uint64_t before = proxy->ref_get_instr_count();
  proxy->ref_exec(requested);
  const uint64_t actual = proxy->ref_get_instr_count() - before;
  executed_instrs += actual;
  if (actual != requested) {
    proxy->sync();
    fprintf(stderr, "FAST short execution: requested=%lu completed=%lu PC=0x%lx\n", (unsigned long)requested,
            (unsigned long)actual, (unsigned long)proxy->state.pc);
    return false;
  }
  return true;
}

template <typename State, typename CommitData>
RefDriveExecutor::Result RefDriveExecutor::run_state(const State &dut, CommitData commit_data) {
  Result result;
#ifdef CONFIG_DIFFTEST_LRSCEVENT
  if (dut.lrsc.valid) {
    if (!flush())
      return {ERROR};
    SyncState sync = {uint64_t(!dut.lrsc.success)};
    proxy->uarchstatus_sync(&sync);
  }
#endif
#ifdef CONFIG_DIFFTEST_NONREGINTERRUPTPENDINGEVENT
  if (dut.non_reg_interrupt_pending.valid) {
    if (!flush())
      return {ERROR};
    const auto &p = dut.non_reg_interrupt_pending;
    NonRegInterruptPending pending = {bool(p.platformIRPMeip),  bool(p.platformIRPMtip),
                                      bool(p.platformIRPMsip),  bool(p.platformIRPSeip),
                                      bool(p.platformIRPStip),  bool(p.platformIRPVseip),
                                      bool(p.platformIRPVstip), bool(p.fromAIAMeip),
                                      bool(p.fromAIASeip),      bool(p.localCounterOverflowInterruptReq)};
    proxy->non_reg_interrupt_pending(pending);
  }
#endif
#ifdef CONFIG_DIFFTEST_MHPMEVENTOVERFLOWEVENT
  if (dut.mhpmevent_overflow.valid) {
    if (!flush())
      return {ERROR};
    proxy->mhpmevent_overflow(dut.mhpmevent_overflow.mhpmeventOverflow);
  }
#endif
#ifdef CONFIG_DIFFTEST_CRITICALERROREVENT
  if (dut.critical_error.valid) {
    if (!flush())
      return {ERROR};
    return {proxy->raise_critical_error() == dut.critical_error.criticalError ? CRITICAL_MATCH : CRITICAL_MISMATCH};
  }
#endif
#ifdef CONFIG_DIFFTEST_SYNCAIAEVENT
  if (dut.sync_aia.valid) {
    if (!flush())
      return {ERROR};
    const auto &a = dut.sync_aia;
    FromAIA aia = {a.mtopei, a.stopei, a.vstopei, a.hgeip};
    proxy->sync_aia(aia);
  }
#endif
#ifdef CONFIG_DIFFTEST_SYNCCUSTOMMFLUSHPWREVENT
  if (dut.sync_custom_mflushpwr.valid) {
    if (!flush())
      return {ERROR};
    proxy->sync_custom_mflushpwr(dut.sync_custom_mflushpwr.l2FlushDone);
  }
#endif
  for (int i = 0; i < CONFIG_DIFF_COMMIT_WIDTH; ++i) {
    if (dut.event.valid && i == dut.event.isLatter) {
      if (!flush())
        return {ERROR};
      const auto &e = dut.event;
      if (e.interrupt) {
        if (e.hasNMI)
          proxy->trigger_nmi(true);
        else if (e.virtualInterruptIsHvictlInject)
          proxy->virtual_interrupt_is_hvictl_inject(true);
        InterruptDelegate delegation = {bool(e.irToHS), bool(e.irToVS)};
        proxy->intr_delegate(delegation);
        proxy->raise_intr(e.interrupt | (1ULL << 63));
      } else if (ref_exception_is_guided(e.exception)) {
        ExecutionGuide guide = {};
        guide.force_raise_exception = true;
        guide.exception_num = e.exception;
        guide.mtval = dut.regs.csr.mtval;
        guide.stval = dut.regs.csr.stval;
#ifdef CONFIG_DIFFTEST_HCSRSTATE
        guide.mtval2 = dut.regs.hcsr.mtval2;
        guide.htval = dut.regs.hcsr.htval;
        guide.vstval = dut.regs.hcsr.vstval;
#endif
        proxy->guided_exec(guide);
      } else {
        proxy->ref_exec(1);
      }
      result.arch_event = true;
      break;
    }
    const auto &c = dut.commit[i];
    if (!c.valid)
      continue;
    result.committed_instrs += 1 + c.nFused;
    if (c.skip) {
      if (!flush())
        return {ERROR};
      proxy->skip_one(c.isRVC, c.rfwen && c.wdest != 0, c.fpwen, c.vecwen, c.wdest, commit_data(i));
    } else if (!execute(1 + c.nFused)) {
      return {ERROR};
    }
  }
  if (dut.trap.hasTrap && !flush())
    return {ERROR};
  return result;
}

RefDriveExecutor::Result RefDriveExecutor::run(const DiffTestState &state) {
  return run_state(state, [&state](int i) { return get_commit_data(&state, i); });
}

RefDriveExecutor::Result RefDriveExecutor::run(const DiffRefDriveState &state, const uint64_t commit_data[]) {
  return run_state(state, [commit_data](int i) { return commit_data[i]; });
}
#endif
