/***************************************************************************************
* Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2025-2026 Beijing Institute of Open Source Chip
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

#include "checkers.h"

#ifdef CONFIG_DIFFTEST_LRSCEVENT
int LrScChecker::check(const DifftestLrScEvent &probe) {
  // sync lr/sc reg microarchitectural status to the REF
  struct SyncState sync;
  sync.sc_fail = !probe.success;
  proxy->uarchstatus_sync((uint64_t *)&sync);
  return STATE_OK;
}
#endif // CONFIG_DIFFTEST_LRSCEVENT

#ifdef CONFIG_DIFFTEST_NONREGINTERRUPTPENDINGEVENT
int NonRegInterruptPendingChecker::check(const DifftestNonRegInterruptPendingEvent &probe) {
  proxy->non_reg_interrupt_pending(probe);
  return STATE_OK;
}
#endif // CONFIG_DIFFTEST_NONREGINTERRUPTPENDINGEVENT

#ifdef CONFIG_DIFFTEST_SYNCAIAEVENT
int AiaChecker::check(const DifftestSyncAIAEvent &probe) {
  proxy->sync_aia(probe);
  return STATE_OK;
}
#endif // CONFIG_DIFFTEST_SYNCAIAEVENT

#ifdef CONFIG_DIFFTEST_MHPMEVENTOVERFLOWEVENT
int MhpmeventOverflowChecker::check(const DifftestMhpmeventOverflowEvent &probe) {
  proxy->mhpmevent_overflow(probe.mhpmeventOverflow);
  return STATE_OK;
}
#endif // CONFIG_DIFFTEST_MHPMEVENTOVERFLOWEVENT

#ifdef CONFIG_DIFFTEST_SYNCCUSTOMMFLUSHPWREVENT
int CustomMflushpwrChecker::check(const DifftestSyncCustomMflushpwrEvent &probe) {
  proxy->sync_custom_mflushpwr(probe.l2FlushDone);
  return STATE_OK;
}
#endif // CONFIG_DIFFTEST_SYNCCUSTOMMFLUSHPWREVENT
