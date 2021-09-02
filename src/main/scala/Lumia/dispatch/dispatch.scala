package Lumia.dispatch

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import Lumia.iq._
import Lumia.rename._
import Lumia.utils._

class DispatchResp(implicit p: Parameters) extends RenameResp {
    val rob_id = UInt(width = robIdBits)
    val st_val = Bool()
    val stq_id = UInt(width = stqIdBits)
    val pcq_val = Bool()
    val pcq_id = UInt(width = pcqIdBits)
    val paq_val = Bool()
    val paq_id = UInt(width = paqIdBits)
}

class DispatchIO(implicit p: Parameters) extends LumiaBundle {
    val kill = new Bundle() {
        val trap_kill = Bool(INPUT)
        val bjp_kill = Valid(new Bundle() {
            val rob_id = UInt(width = robIdBits)
            val stq_id = UInt(width = stqIdBits)
            val pcq_val = Bool()
            val pcq_id = UInt(width = pcqIdBits)
            val paq_val = Bool()
            val paq_id = UInt(width = paqIdBits)
        }).flip
    }
    val stall = Bool(INPUT)
    val sync_csr_ret = Bool(INPUT)
    val sync_fence_ret = Bool(INPUT)
    val csr_ret = Bool(INPUT)
    val fence_ret = Bool(INPUT)
    val req = Vec(decodeWidth, Valid(new RenameResp).flip)
    val fire_req = Vec(retireWidth, new Bundle() {
        val fire = Bool(INPUT)
        val st_val = Bool(INPUT)
        val pcq_val = Bool(INPUT)
        val paq_val = Bool(INPUT)
    })
    val stq_exc_done = Bool(INPUT)

    val resp = Vec(dispatchWidth, Valid(new DispatchResp))
    val rob_dsp_ptr = UInt(OUTPUT, width = robIdBits)
    val rob_ret_ptr = UInt(OUTPUT, width = robIdBits)
    val stq_dsp_ptr = UInt(OUTPUT, width = stqIdBits)
    val stq_exc_ptr = UInt(OUTPUT, width = stqIdBits)
    val stq_ret_ptr = UInt(OUTPUT, width = stqIdBits)
    val dsp_stall = Bool(OUTPUT)
}

class Dispatch(implicit p: Parameters) extends LumiaModule {
    val io = IO(new DispatchIO)
    val kill = io.kill.trap_kill || io.kill.bjp_kill.valid
    val robQueue = Module(new RobQueue).io
    val storeQueue = Module(new StoreQueue).io
    val pcqQueue = Module(new PCQ).io
    val paqQueue = Module(new PAQ).io

    //  Detect
    val csr_stall_detect = (io.req(0).valid
      && io.req(0).bits.uop.fu === FU_ALU
      && (io.req(0).bits.uop.uop === uopCSRRC
      ||  io.req(0).bits.uop.uop === uopCSRRS
      ||  io.req(0).bits.uop.uop === uopCSRRW))
    val fence_stall_detect = (io.req(0).valid
      && io.req(0).bits.uop.fu === FU_ALU
      && (io.req(0).bits.uop.uop === uopFENCE
      || io.req(0).bits.uop.uop === uopVMA))

    val stall_flag_set = csr_stall_detect || fence_stall_detect
    val stall_flag_clr_1 = io.sync_csr_ret || io.sync_fence_ret
    val stall_flag_clr_2 = io.csr_ret || io.fence_ret
    //
    val s_ready::s_stall_1::s_stall_2::Nil = Enum(3)
    val state = Reg(init = s_ready)
    val state_nxt = Wire(UInt(s_ready.getWidth.W))
    when (kill) { state_nxt := s_ready }
      .elsewhen (stall_flag_set && state === s_ready)       { state_nxt := s_stall_1    }
      .elsewhen (stall_flag_clr_1 && state === s_stall_1)   { state_nxt := s_stall_2    }
      .elsewhen (stall_flag_clr_2 && state === s_stall_2)   { state_nxt := s_ready      }
      .otherwise                                            { state_nxt := state        }
    state := state_nxt
    //
    val not_enough_space = robQueue.empty || storeQueue.empty || pcqQueue.empty || paqQueue.empty
    //  Rob queue
    robQueue.kill.trap_kill := io.kill.trap_kill
    robQueue.kill.bjp_kill := io.kill.bjp_kill.valid
    robQueue.kill.bjp_rob_id := io.kill.bjp_kill.bits.rob_id
    robQueue.alloc_req := io.req.map(_.valid)
    robQueue.fire_req := io.fire_req.map(_.fire)
    robQueue.stall := io.dsp_stall

    //  Store queue
    storeQueue.kill.trap_kill := io.kill.trap_kill
    storeQueue.kill.bjp_kill := io.kill.bjp_kill.valid
    storeQueue.kill.bjp_stq_id := io.kill.bjp_kill.bits.stq_id
    storeQueue.alloc_req := io.req.map(s => s.valid && (s.bits.uop.fu === FU_LSU || s.bits.uop.uop === uopSTORE))
    storeQueue.fire_req := io.fire_req.map(s => s.fire && s.st_val)
    storeQueue.stall := io.dsp_stall

    //  PCQ
    val pcq_addr_cmp = Reg(init = UInt(0xfffff, width = vaddrBits - 12))
    val addr_sep = pcq_addr_cmp +: io.req.map(_.bits.addr(vaddrBits - 1, 12))
    val pcq_req = Wire(Vec(dispatchWidth, Bool()))
    for (i <- 0 until dispatchWidth) {
        pcq_req(i) := (addr_sep(i) =/= addr_sep(i + 1)) & io.req(i).valid
    }
    pcqQueue.kill.trap_kill := io.kill.trap_kill
    pcqQueue.kill.bjp_kill := io.kill.bjp_kill.valid
    pcqQueue.kill.bjp_pcq_val := io.kill.bjp_kill.bits.pcq_val
    pcqQueue.kill.bjp_pcq_id := io.kill.bjp_kill.bits.pcq_id
    pcqQueue.alloc_req := pcq_req
    pcqQueue.fire_req := io.fire_req.map(s => s.fire & s.pcq_val)
    pcqQueue.stall := io.dsp_stall

    val select_last = select_first(Cat(io.req.map(_.valid)), 1).map(OHToUInt(_))
    val ren_req = io.req.map(_.valid).reduce(_|_)
    when (io.kill.trap_kill) {
        pcq_addr_cmp := UInt(0xfffff)
    } .elsewhen (ren_req & !io.stall) {
        pcq_addr_cmp := io.req(select_last(0)).bits.addr(vaddrBits - 1, 12)
    }

    //  PAQ
    val paq_req = io.req.map(s => s.valid && s.bits.s2_bpu_pred.taken)
    paqQueue.kill.trap_kill := io.kill.trap_kill
    paqQueue.kill.bjp_kill := io.kill.bjp_kill.valid
    paqQueue.kill.bjp_paq_val := io.kill.bjp_kill.bits.paq_val
    paqQueue.kill.bjp_paq_id := io.kill.bjp_kill.bits.paq_id
    paqQueue.alloc_req := paq_req
    paqQueue.fire_req := io.fire_req.map(s => s.fire & s.paq_val)
    paqQueue.stall := io.dsp_stall

    //
    val sync_csr_uops = spec_uop(uopSYNC)
    val sync_fence_uops = spec_uop(uopSYNF)
    val dsp_resp = Array.fill(dispatchWidth){Reg(Valid(new DispatchResp))}
    for (dw <- 0 until dispatchWidth) {
        when (kill) { dsp_resp(dw).valid := false.B }
          .elsewhen (io.req(dw).valid && !io.stall && !not_enough_space) {
              when (state === s_ready)                              { dsp_resp(dw).valid := true.B  } //  First
                .elsewhen (state === s_stall_1 && stall_flag_clr_1) { dsp_resp(dw).valid := true.B  } //  Second
                .otherwise                                          { dsp_resp(dw).valid := false.B }
          } .otherwise { dsp_resp(dw).valid := false.B }
        //  First
        when (state === s_ready ) {
            dsp_resp(dw).bits.uop := Mux(csr_stall_detect || fence_stall_detect
                ,   Mux(csr_stall_detect, sync_csr_uops, sync_fence_uops)
                ,   io.req(dw).bits.uop)
        }
        //  Second
        when (state === s_stall_1 && stall_flag_clr_1) {
            dsp_resp(dw).bits.uop := io.req(dw).bits.uop
        }
        dsp_resp(dw).bits.rob_id  := robQueue.alloc_rob_id(dw)
        dsp_resp(dw).bits.st_val  := io.req(dw).valid  && (io.req(dw).bits.uop.fu === FU_LSU
          && (io.req(dw).bits.uop.uop === uopSTORE
          ||  io.req(dw).bits.uop.uop === uopFLW
          ||  io.req(dw).bits.uop.uop === uopFLD))
        dsp_resp(dw).bits.stq_id    := storeQueue.alloc_stq_id(dw)
        dsp_resp(dw).bits.pcq_val   := pcq_req(dw)
        dsp_resp(dw).bits.pcq_id    := pcqQueue.alloc_pcq_id(dw)
        dsp_resp(dw).bits.paq_val   := paq_req(dw)
        dsp_resp(dw).bits.paq_id    := paqQueue.alloc_paq_id(dw)
        dsp_resp(dw).bits.addr      := io.req(dw).bits.addr
        dsp_resp(dw).bits.taddr     := io.req(dw).bits.taddr
        dsp_resp(dw).bits.len       := io.req(dw).bits.len
        dsp_resp(dw).bits.cause     := io.req(dw).bits.cause
        dsp_resp(dw).bits.s2_bpu_pred := io.req(dw).bits.s2_bpu_pred
        dsp_resp(dw).bits.gc_tag      := io.req(dw).bits.gc_tag
    }

    io.resp <> dsp_resp
    //
    io.rob_dsp_ptr := robQueue.dsp_ptr
    io.rob_ret_ptr := robQueue.ret_ptr
    io.stq_dsp_ptr := storeQueue.stq_dsp_ptr
    io.stq_exc_ptr := storeQueue.stq_exc_ptr
    io.stq_ret_ptr := storeQueue.stq_ret_ptr

    //  Stall
    io.dsp_stall := io.stall || not_enough_space || ((csr_stall_detect || fence_stall_detect) && state_nxt =/= s_ready)
}