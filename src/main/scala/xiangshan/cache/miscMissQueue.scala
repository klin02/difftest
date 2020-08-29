package xiangshan.cache

import chisel3._
import chisel3.util._

import utils.XSDebug

// wraps around MiscPipe
// when requests misse, send miss req to missQueue and replays reqs
class MiscMissQueue extends DCacheModule
{
  val io = IO(new DCacheBundle {
    val lsu         = Flipped(new DCacheLoadIO)
    val replay      = new DCacheLoadIO
    val miss_req    = DecoupledIO(new MissReq)
    val miss_resp   = Flipped(ValidIO(new MissResp))
    val miss_finish = DecoupledIO(new MissFinish)
  })

  val s_invalid :: s_replay_req :: s_replay_resp :: s_resp :: s_miss_req :: s_miss_resp :: s_miss_finish :: Nil = Enum(7)
  val state = RegInit(s_invalid)
  val id = 0.U

  val req     = Reg(new DCacheWordReq)
  val resp    = Reg(new DCacheResp)
  val req_block_addr = get_block_addr(req.addr)
  val reg_miss_resp = Reg(new MissResp)

  // assign default values to output signals
  io.lsu.req.ready     := state === s_invalid
  io.lsu.resp.valid    := false.B
  io.lsu.resp.bits     := DontCare

  io.replay.req.valid  := false.B
  io.replay.req.bits   := DontCare
  io.replay.resp.ready := false.B
  io.replay.s1_kill    := false.B

  io.miss_req.valid    := false.B
  io.miss_req.bits     := DontCare
  io.miss_finish.valid := false.B
  io.miss_finish.bits  := DontCare


  XSDebug("state: %d\n", state)
  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    when (io.lsu.req.fire()) {
      assert(!io.lsu.req.bits.meta.replay)
      req   := io.lsu.req.bits
      state := s_replay_req
    }
  }

  // --------------------------------------------
  // replay
  when (state === s_replay_req) {
    io.replay.req.valid := true.B
    io.replay.req.bits  := req
    when (io.replay.req.fire()) {
      state := s_replay_resp
    }
  }

  when (state === s_replay_resp) {
    io.replay.resp.ready := true.B
    when (io.replay.resp.fire()) {
      when (io.replay.resp.bits.miss) {
        // replayed reqs should not miss
        assert(!req.meta.replay)
        when (!req.meta.replay) {
          state := s_miss_req
        }
      } .otherwise {
        resp := io.replay.resp.bits
        when (!req.meta.replay) {
          state := s_resp
        } .otherwise {
          state := s_miss_finish
        }
      }

      assert(!io.replay.resp.bits.nack)
    }
  }

  // --------------------------------------------
  when (state === s_miss_req) {
    io.miss_req.valid          := true.B
    io.miss_req.bits.cmd       := req.cmd
    io.miss_req.bits.addr      := req_block_addr
    io.miss_req.bits.client_id := id

    when (io.miss_req.fire()) {
      state := s_miss_resp
    }
  }

  when (state === s_miss_resp) {
    when (io.miss_resp.fire()) {
      reg_miss_resp   := io.miss_resp.bits
      // mark req as replayed req
      req.meta.replay := true.B
      state           := s_replay_req
    }
  }

  when (state === s_miss_finish) {
    io.miss_finish.valid          := true.B
    io.miss_finish.bits.client_id := id
    io.miss_finish.bits.entry_id  := reg_miss_resp.entry_id
    when (io.miss_finish.fire()) {
      state := s_resp
    }
  }

  // --------------------------------------------
  when (state === s_resp) {
    io.lsu.resp.valid := true.B
    io.lsu.resp.bits  := resp

    when (io.lsu.resp.fire()) {
      state := s_invalid
    }
  }
}