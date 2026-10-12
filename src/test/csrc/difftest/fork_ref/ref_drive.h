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
#ifndef DIFFTEST_REF_DRIVE_H
#define DIFFTEST_REF_DRIVE_H
#include "refproxy.h"

void ref_drive_init(RefProxy &proxy, const DiffTestRegState &regs);

#ifdef CONFIG_DIFFTEST_FAST_REF
class RefDriveExecutor {
public:
  enum Status {
    OK,
    ERROR,
    CRITICAL_MATCH,
    CRITICAL_MISMATCH
  };
  struct Result {
    Status status = OK;
    uint32_t committed_instrs = 0;
    bool arch_event = false;
  };
  explicit RefDriveExecutor(RefProxy *proxy) : proxy(proxy) {}
  Result run(const DiffTestState &state);
  Result run(const DiffRefDriveState &state, const uint64_t commit_data[]);
  bool execute(uint64_t instr_count);
  bool flush();
  uint64_t completed() const {
    return executed_instrs;
  }

private:
  template <typename State, typename CommitData> Result run_state(const State &state, CommitData commit_data);
  static constexpr uint64_t MAX_PENDING = 65536;
  RefProxy *proxy;
  uint64_t pending_instrs = 0;
  uint64_t executed_instrs = 0;
};
#endif
#endif
