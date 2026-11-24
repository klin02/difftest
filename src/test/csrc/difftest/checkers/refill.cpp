/***************************************************************************************
* Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2025 Beijing Institute of Open Source Chip
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
#include "goldenmem.h"

#ifdef CONFIG_DIFFTEST_REFILLEVENT
bool RefillChecker::get_valid(const DifftestRefillEvent &probe) {
  return probe.valid;
}

void RefillChecker::clear_valid(DifftestRefillEvent &probe) {
  probe.valid = 0;
}

#ifdef CONFIG_DIFFTEST_CMOINVALEVENT
static void sync_cmo_refill(DiffState *state, RefProxy *proxy, uint64_t line, const DifftestRefillEvent &probe,
                            uint64_t written, bool ref_watch) {
  auto cmo = state->cmo_inval_lines.find(line);
  assert(cmo != state->cmo_inval_lines.end());
  uint64_t refill_bytes = 0;
  for (int beat = 0; beat < 8; beat++) {
    if (probe.mask & (1U << beat)) {
      refill_bytes |= UINT64_C(0xff) << (beat * 8);
    }
  }

  const uint64_t sync = refill_bytes & ~written & ~cmo->second.seen;
  cmo->second.seen |= refill_bytes;
  const auto *data = reinterpret_cast<const uint8_t *>(probe.data);
  // Keep mismatch-only synchronization for legacy REFs without a store mask.
  bool needs_sync = ref_watch || written != 0;
  for (int byte = 0; !needs_sync && byte < 64; byte++) {
    if ((sync >> byte) & 1) {
      uint64_t gold;
      read_goldenmem(line + byte, &gold, 1);
      needs_sync = static_cast<uint8_t>(gold) != data[byte];
    }
  }
  if (sync != 0 && needs_sync) {
    update_goldenmem(line, const_cast<uint8_t *>(data), sync, 64);
    for (int byte = 0; byte < 64;) {
      if (((sync >> byte) & 1) == 0) {
        byte++;
        continue;
      }
      int start = byte++;
      while (byte < 64 && ((sync >> byte) & 1)) {
        byte++;
      }
      proxy->ref_memcpy(line + start, const_cast<uint8_t *>(data + start), byte - start, DUT_TO_REF);
    }
    Info("INFO: Sync GoldenMem using refill Data from DUT (Because of CBO.INVAL):\n");
    Info("      addr: %lx, kept=%016lx, sync=%016lx\n", line, written, sync);
  }
  if (cmo->second.seen == UINT64_MAX) {
    if (ref_watch) {
      proxy->ref_cmo_inval_release(line);
    }
    state->cmo_inval_lines.erase(cmo);
  }
}

#ifdef DEBUG_REFILL
int RefillChecker::sync_early_cmo_refills(DiffState *state, RefProxy *proxy) {
  for (auto it = state->early_cmo_refills.begin(); it != state->early_cmo_refills.end();) {
    uint64_t written;
    switch (proxy->cmo_inval_written(it->first, written)) {
      case RefProxy::CmoWatchState::Armed: ++it; break;
      case RefProxy::CmoWatchState::Active:
        sync_cmo_refill(state, proxy, it->first, it->second, written, true);
        it = state->early_cmo_refills.erase(it);
        break;
      default: Info("Missing REF CBO.INVAL watch for line 0x%lx\n", it->first); return STATE_DIFF;
    }
  }
  return STATE_OK;
}
#endif
#endif // CONFIG_DIFFTEST_CMOINVALEVENT

int RefillChecker::check(const DifftestRefillEvent &probe) {
  static int delay = 0;
  delay = delay * 2;
  if (delay > 16) {
    return STATE_ERROR;
  }
  static uint64_t last_valid_addr = 0;
  char buf[512];
  char flag_buf[512];
  uint64_t realpaddr = probe.addr;
  uint64_t paddr = probe.addr - probe.addr % 64;

#ifdef CONFIG_DIFFTEST_CMOINVALEVENT
  auto cmo = state->cmo_inval_lines.find(paddr);
  if (cmo != state->cmo_inval_lines.end()) {
    last_valid_addr = paddr;
    uint64_t written = cmo->second.written;
    const bool ref_watch = proxy->has_cmo_write_watch();
#ifdef DEBUG_REFILL
    if (ref_watch) {
      switch (proxy->cmo_inval_written(paddr, written)) {
        case RefProxy::CmoWatchState::Active: break;
        case RefProxy::CmoWatchState::Armed: {
          // Normal checkers run before REF commit. Apply this data once the REF retires the CBO.
          auto &early = state->early_cmo_refills[paddr];
          for (int beat = 0; beat < 8; beat++) {
            if ((probe.mask & ~early.mask) & (1U << beat)) {
              early.data[beat] = probe.data[beat];
            }
          }
          early.mask |= probe.mask;
          return STATE_OK;
        }
        default: Info("Missing REF CBO.INVAL watch for line 0x%lx\n", paddr); return STATE_DIFF;
      }
    }
#endif
    sync_cmo_refill(state, proxy, paddr, probe, written, ref_watch);
    return STATE_OK;
  }
#endif // CONFIG_DIFFTEST_CMOINVALEVENT

  if (paddr != last_valid_addr) {
    last_valid_addr = paddr;
    if (!in_pmem(paddr)) {
      // speculated illegal mem access should be ignored
      return STATE_OK;
    }
    for (int i = 0; i < 8; i++) {
      read_goldenmem(paddr + i * 8, &buf, 8, &flag_buf);
      if ((probe.mask & (1 << i)) && probe.data[i] != *((uint64_t *)buf)) {
#ifdef CONFIG_DIFFTEST_UNCACHEMMSTOREEVENT
        // in multi-core, uncache mm store may cause data inconsistencies.
        // so here needs to override the nemu value with the dut value by cacheline granularity.
        if (*((uint64_t *)flag_buf) != 0) {
          Info("INFO: Sync GoldenMem using refill Data from DUT (Because of uncache main-mem store):\n");
          Info("      cacheid=%d, addr: %lx\n      Gold: ", index, paddr);
          for (int j = 0; j < 8; j++) {
            read_goldenmem(paddr + j * 8, &buf, 8);
            Info("%016lx", *((uint64_t *)buf));
          }
          Info("\n      Core: ");
          for (int j = 0; j < 8; j++) {
            Info("%016lx", probe.data[j]);
          }
          Info("\n");
          update_goldenmem(paddr, (void *)probe.data, 0xffffffffffffffffUL, 64);
          proxy->ref_memcpy(paddr, (void *)probe.data, 64, DUT_TO_REF);
          return STATE_OK;
        }
#endif // CONFIG_DIFFTEST_UNCACHEMMSTOREEVENT
        Info("cacheid=%d,mask=%x,realpaddr=0x%lx: Refill test failed!\n", index, probe.mask, realpaddr);
        Info("addr: %lx\nGold: ", paddr);
        for (int j = 0; j < 8; j++) {
          read_goldenmem(paddr + j * 8, &buf, 8);
          Info("%016lx", *((uint64_t *)buf));
        }
        Info("\nCore: ");
        for (int j = 0; j < 8; j++) {
          Info("%016lx", probe.data[j]);
        }
        Info("\n");
        // continue run some cycle before aborted to dump wave
        if (delay == 0) {
          delay = 1;
        }
        return STATE_OK;
      }
    }
  }

  return STATE_OK;
}

#ifdef CONFIG_DIFFTEST_CMOINVALEVENT
bool CmoInvalRecorder::get_valid(const DifftestCMOInvalEvent &probe) {
  return probe.valid;
}

void CmoInvalRecorder::clear_valid(DifftestCMOInvalEvent &probe) {
  probe.valid = 0;
}

int CmoInvalRecorder::check(const DifftestCMOInvalEvent &probe) {
  const uint64_t line = probe.addr & ~UINT64_C(63);
  if (!in_pmem(line)) {
    return STATE_OK;
  }
  if (proxy->has_cmo_write_watch() && !proxy->ref_cmo_inval_arm(line)) {
    Info("Cannot arm REF CBO.INVAL watch for line 0x%lx\n", line);
    return STATE_DIFF;
  }
  state->cmo_inval_lines[line] = {};
  return STATE_OK;
}
#endif // CONFIG_DIFFTEST_CMOINVALEVENT

#endif // CONFIG_DIFFTEST_REFILLEVENT
