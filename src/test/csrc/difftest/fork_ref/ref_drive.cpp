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

#ifdef CONFIG_DIFFTEST_FAST_REF
#include "checkers.h"
#include "diffstate.h"
#include <algorithm>

// Keep these causes consistent with ArchEventChecker::do_exception.
enum {
  EX_IPF = 12,
  EX_LPF = 13,
  EX_SPF = 15,
  EX_HWE = 19,
  EX_IGPF = 20,
  EX_LGPF = 21,
  EX_SGPF = 23
};

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

template <typename Checker, typename Probe> bool RefDriveExecutor::sync(Probe &probe) {
  return !probe.valid || Checker({}, nullptr, proxy).run(probe, this) == DiffTestChecker::STATE_OK;
}

template <typename State, typename CommitData>
RefDriveExecutor::Result RefDriveExecutor::run_state(State &dut, CommitData commit_data) {
  Result result;
#ifdef CONFIG_DIFFTEST_LRSCEVENT
  if (!sync<LrScChecker>(dut.lrsc))
    return {ERROR};
#endif
#ifdef CONFIG_DIFFTEST_NONREGINTERRUPTPENDINGEVENT
  if (!sync<NonRegInterruptPendingChecker>(dut.non_reg_interrupt_pending))
    return {ERROR};
#endif
#ifdef CONFIG_DIFFTEST_MHPMEVENTOVERFLOWEVENT
  if (!sync<MhpmeventOverflowChecker>(dut.mhpmevent_overflow))
    return {ERROR};
#endif
#ifdef CONFIG_DIFFTEST_CRITICALERROREVENT
  if (dut.critical_error.valid) {
    if (!flush())
      return {ERROR};
    const bool match = proxy->raise_critical_error() == dut.critical_error.criticalError;
    if constexpr (!std::is_const_v<State>)
      dut.critical_error.valid = 0;
    return {match ? CRITICAL_MATCH : CRITICAL_MISMATCH};
  }
#endif
#ifdef CONFIG_DIFFTEST_SYNCAIAEVENT
  if (!sync<AiaChecker>(dut.sync_aia))
    return {ERROR};
#endif
#ifdef CONFIG_DIFFTEST_SYNCCUSTOMMFLUSHPWREVENT
  if (!sync<CustomMflushpwrChecker>(dut.sync_custom_mflushpwr))
    return {ERROR};
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
      } else if (e.exception == EX_IPF || e.exception == EX_LPF || e.exception == EX_SPF || e.exception == EX_IGPF ||
                 e.exception == EX_LGPF || e.exception == EX_SGPF || e.exception == EX_HWE) {
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

RefDriveExecutor::Result RefDriveExecutor::run(DiffTestState &state) {
  return run_state(state, [&state](int i) { return get_commit_data(&state, i); });
}

RefDriveExecutor::Result RefDriveExecutor::run(const DiffTestState &state, const uint64_t commit_data[]) {
  return run_state(state, [commit_data](int i) { return commit_data[i]; });
}
#endif
