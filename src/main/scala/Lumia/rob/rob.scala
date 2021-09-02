package Lumia.rob

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.{Causes, DecodeLogic}
import Lumia.dispatch._
import Lumia.cache._
import Lumia.exu._
import Lumia.issue._
import Lumia.common._
import Lumia.utils._

class RobData(implicit p: Parameters) extends LumiaBundle {
    val st_val  = Bool()
    val st_id   = UInt(width = stqIdBits)
    val pcq_val = Bool()
    val pcq_id  = UInt(width = pcqIdBits)
    val paq_val = Bool()
    val paq_id  = UInt(width = paqIdBits)
    val gc_tag  = UInt(width = gcTagSz)
    val inst    = new Bundle() {
        val ecall   = Bool()
        val ebreak  = Bool()
        val mret    = Bool()
        val sret    = Bool()
        val uret    = Bool()
        val dret    = Bool()
        val wfi     = Bool()
        val sfence  = Bool()
        val csr     = Bool()
        val sync_csr = Bool()
        val sync_fence = Bool()
        val fence   = Bool()
        val fencei  = Bool()
        val excep   = Bool()
    }
    val len     = Bool()
}


class RobFireReq(implicit p: Parameters) extends LumiaBundle {
    val gc_tag  = UInt(width = gcTagSz)
    val st_val  = Bool()
    val st_id   = UInt(width = stqIdBits)
    val pcq_val = Bool()
    val pcq_id  = UInt(width = pcqIdBits)
    val paq_val = Bool()
    val paq_id  = UInt(width = paqIdBits)
}

class RobException(implicit p: Parameters) extends LumiaBundle {
    val ecall   = Bool()
    val ebreak  = Bool()
    val mret    = Bool()
    val sret    = Bool()
    val uret    = Bool()
    val dret    = Bool()
    val wfi     = Bool()
    val sfence  = Bool()

    val exception = Bool()
    val exc_len   = Bool()
    val exc_cause = UInt(width = xLen)
    val exc_pc    = UInt(width = vaddrBits)
    val badvaddr  = UInt(width = xLen)
}


class RobIO(implicit p: Parameters) extends LumiaBundle {
    val kill = new Bundle() {
        val trap_kill = Bool(INPUT)
        val bjp_kill = Bool(INPUT)
        val bjp_rob_id = UInt(INPUT, width = robIdBits)
        val bjp_pcq_id = UInt(OUTPUT, width = pcqIdBits)
        val bjp_paq_id = UInt(OUTPUT, width = paqIdBits)
        val bjp_addr_lo = UInt(OUTPUT, width = 12)

        val itlb_kill = DecoupledIO(new SFenceReq)
        val dtlb_kill = DecoupledIO(new SFenceReq)
        val mmu_kill = DecoupledIO(new SFenceReq)

        val sfence_kill = Bool(OUTPUT)
    }
    //  Enq
    val enq = Vec(dispatchWidth, Valid(new DispatchResp).flip)
    val rob_dsp_ptr = UInt(INPUT, width = robIdBits)
    val rob_ret_ptr = UInt(INPUT, width = robIdBits)

    //  Exec done
    val exc_done = Vec(issueWidth, Bool(INPUT))
    val exc_rob_id = Vec(issueWidth, UInt(INPUT, width = robIdBits))
    val cause = Vec(issueWidth, UInt(INPUT, width = xLen))
    val sfence = new SFenceReq().asInput

    //  Retire
    val fire_req = Vec(retireWidth, Valid(new RobFireReq))
    val retire = UInt(OUTPUT, width = log2Ceil(1 + retireWidth))
    val retire_cause = new RobException().asOutput

    //  CSR & Fence
    val csr_stall = Bool(INPUT)
    val sync_csr_ret = Bool(OUTPUT)
    val sync_fence_ret = Bool(OUTPUT)
    val csr_ret = Bool(OUTPUT)
    val fence_ret = Bool(OUTPUT)

    //  Force write
    val force_write = Bool(OUTPUT)
    val force_write_done = Bool(INPUT)

    //  Interrupts
    val interrupt_val = Bool(INPUT)

    //  Address ports
    val exu_rob_id = Vec(3, UInt(INPUT, width = robIdBits))
    val exu_addr_lo = Vec(3, UInt(OUTPUT, width = 12))
    val trap_pcq_id = UInt(OUTPUT, width = pcqIdBits)
    val trap_addr_hi = UInt(INPUT, width = vaddrBits - 12)
}

trait HasRobCtrlSigs {
    val ecall   = Bool()
    val ebreak  = Bool()
    val mret    = Bool()
    val sret    = Bool()
    val uret    = Bool()
    val dret    = Bool()
    val wfi     = Bool()
    val sfence  = Bool()
    val csr     = Bool()
    val sync_csr = Bool()
    val sync_fence = Bool()
    val fence   = Bool()
    val fencei  = Bool()
    val excep   = Bool()
}

class RobCtrlSigs extends Bundle with HasRobCtrlSigs

class RobDecoder(nPorts: Int)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new Bundle() {
        val fu = Vec(nPorts, Bool(INPUT))
        val uop = Vec(nPorts, UInt(INPUT, width = uopBits))
        val sigs = Vec(nPorts, new RobCtrlSigs().asOutput)
    })

    val default = List[BitPat](X, X, X, X, X, X, X, X, X, X, X, X, X, X)
    val insns = Array[(BitPat, List[BitPat])](
        BitPat(uopECALL)    -> List(Y, N, N, N, N, N, N, N, N, N, N, N, N, N),
        BitPat(uopEBREAK)   -> List(N, Y, N, N, N, N, N, N, N, N, N, N, N, N),
        BitPat(uopMRET)     -> List(N, N, Y, N, N, N, N, N, N, N, N, N, N, N),
        BitPat(uopSRET)     -> List(N, N, N, Y, N, N, N, N, N, N, N, N, N, N),
        BitPat(uopURET)     -> List(N, N, N, N, Y, N, N, N, N, N, N, N, N, N),
        BitPat(uopDRET)     -> List(N, N, N, N, N, Y, N, N, N, N, N, N, N, N),
        BitPat(uopWFI)      -> List(N, N, N, N, N, N, Y, N, N, N, N, N, N, N),
        BitPat(uopVMA)      -> List(N, N, N, N, N, N, N, Y, N, N, N, N, N, N),
        BitPat(uopCSRRC)    -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, N),
        BitPat(uopCSRRS)    -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, N),
        BitPat(uopCSRRW)    -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, N),
        BitPat(uopSYNC)     -> List(N, N, N, N, N, N, N, N, N, Y, N, N, N, N),
        BitPat(uopSYNF)     -> List(N, N, N, N, N, N, N, N, N, N, Y, N, N, N),
        BitPat(uopFENCE)    -> List(N, N, N, N, N, N, N, N, N, N, N, Y, N, N),
        BitPat(uopFENCEI)   -> List(N, N, N, N, N, N, N, N, N, N, N, N, Y, N),
        BitPat(uopEXC)      -> List(N, N, N, N, N, N, N, N, N, N, N, N, N, Y)
    )

    val decoder = io.uop.map(s => DecodeLogic(s, default, insns))
    val s = io.sigs
    val sigs = io.sigs.map(s =>
                Seq(s.ecall, s.ebreak, s.mret, s.sret, s.uret, s.dret
                , s.wfi, s.sfence, s.csr, s.sync_csr, s.sync_fence, s.fence, s.fencei, s.excep))

    for (i <- 0 until nPorts) {
        sigs(i) zip decoder(i) map { case (f, s) => f := s & (io.fu(i) === FU_ALU) }
    }

}

class Rob(implicit p: Parameters) extends LumiaModule {
    val io = IO(new RobIO)

    def genIndex(id: UInt)        = id(robIdBits - 2, 0)
    def constranit(data: RobData) = {
        val res = (data.inst.ecall
          |  data.inst.ebreak
          |  data.inst.mret
          |  data.inst.sret
          |  data.inst.uret
          |  data.inst.dret
          |  data.inst.wfi
          |  data.inst.sync_csr
          |  data.inst.sync_fence
          |  data.inst.csr
          |  data.inst.fence
          |  data.inst.fencei
          |  data.inst.sfence
          |  data.inst.excep)
        res
    }

    //***********************************************
    //  Rob decoding
    val rob_decoder = Module(new RobDecoder(dispatchWidth))
    rob_decoder.io.fu := io.enq.map(_.bits.uop.fu)
    rob_decoder.io.uop := io.enq.map(_.bits.uop.uop)

    //***********************************************
    //  Fields
    val rob_data = Reg(Vec(numRobEntries, new RobData))
    val rob_cause = Reg(Vec(numRobEntries, UInt(width = xLen)))
    val rob_done = Reg(Vec(numRobEntries, Bool()))
    val rob_addr = Reg(Vec(numRobEntries, UInt(width = 12)))
    val rob_fflags = Reg(Vec(numRobEntries, UInt(width = FPUConsts.FLAGS_SZ)))
    val rob_debug_wdata = chisel3.SyncReadMem(numRobEntries, UInt(width = xLen))

    //***********************************************
    //  Enq
    val enq_val = io.enq.map(_.valid)
    val dsp_kill = io.kill.trap_kill || io.kill.bjp_kill
    val new_entries = Wire(Vec(dispatchWidth, new RobData))

    //  Set default
    new_entries.foreach(_ := 0.U.asTypeOf(new RobData()))
    val sigs = rob_decoder.io.sigs
    for (dw <- 0 until dispatchWidth) {
        new_entries(dw).st_val          := io.enq(dw).bits.st_val
        new_entries(dw).st_id           := io.enq(dw).bits.stq_id
        new_entries(dw).pcq_val         := io.enq(dw).bits.pcq_val
        new_entries(dw).pcq_id          := io.enq(dw).bits.pcq_id
        new_entries(dw).paq_val         := io.enq(dw).bits.paq_val
        new_entries(dw).paq_id          := io.enq(dw).bits.paq_id
        new_entries(dw).inst.ecall      := sigs(dw).ecall
        new_entries(dw).inst.ebreak     := sigs(dw).ebreak
        new_entries(dw).inst.mret       := sigs(dw).mret
        new_entries(dw).inst.sret       := sigs(dw).sret
        new_entries(dw).inst.uret       := sigs(dw).uret
        new_entries(dw).inst.dret       := sigs(dw).dret
        new_entries(dw).inst.wfi        := sigs(dw).wfi
        new_entries(dw).inst.csr        := sigs(dw).csr
        new_entries(dw).inst.sync_csr   := sigs(dw).sync_csr
        new_entries(dw).inst.sync_fence := sigs(dw).sync_fence
        new_entries(dw).inst.sfence     := sigs(dw).sfence
        new_entries(dw).inst.fence      := sigs(dw).fence
        new_entries(dw).inst.fencei     := sigs(dw).fencei
        new_entries(dw).inst.excep      := sigs(dw).excep

        when (enq_val(dw) && !dsp_kill(dw)) {
            val pos = genIndex(io.enq(dw).bits.rob_id)
            rob_data(pos) := new_entries(dw)
            rob_addr(pos) := io.enq(dw).bits.addr(11, 0)
            rob_done(pos) := false.B
            rob_fflags(pos) := 0.U
        }
    }

    //***********************************************
    //  Exec done
    for (iw <- 0 until issueWidth) {
        when (io.exc_done(iw)) {
            val pos = genIndex(io.exu_rob_id(iw))
            rob_done(pos) := true.B
            rob_cause(pos) := io.cause(iw)
        }
    }

    //
    val sfence_field = Reg(new SFenceReq)
    when (io.exc_done(0)) { sfence_field := io.sfence }

    //***********************************************
    //  Kill
    when (io.kill.trap_kill) { rob_done.foreach(_ := false.B) }

    //***********************************************
    //  Retire
    val rob_alloc_ptr = io.rob_dsp_ptr
    val rob_ret_ptr = io.rob_ret_ptr

    val ret_data = Wire(Vec(retireWidth, new RobData))
    val ret_cause = rob_cause(genIndex(rob_ret_ptr))
    for (rw <- 0 until retireWidth) {
        val pos = genIndex(rob_ret_ptr + rw.U)
        ret_data(rw) := rob_data(pos)
    }

    //  Select logic
    val rob_constranit = Vec(ret_data.map(s => constranit(s)))
    val fence_stall = ret_data(0).inst.sync_fence & !io.force_write_done

    //  Rob finite state machine
    val s_ret::s_int_chk::Nil = Enum(2)
    val state = Reg(init = s_ret)

    val ret_rdy = Wire(Vec(retireWidth, Bool()))
    ret_rdy(0) := rob_done(genIndex(rob_ret_ptr)) & !rob_cause(genIndex(rob_ret_ptr)).orR()
    for (rw <- 1 until retireWidth) {
        val pos = genIndex(rob_ret_ptr + rw.U)
        ret_rdy(rw) := (ret_rdy(rw - 1)
          & !Cat(rob_constranit.reverse)(rw, 0).orR()
          & rob_done(pos)
          & !rob_done(pos).orR())
    }
    io.retire := PopCount(ret_rdy)

    val ret_fire = Wire(Vec(retireWidth, new RobFireReq))
    ret_fire.zipWithIndex.map { case (v, i) =>
        v.gc_tag    := ret_data(i).gc_tag
        v.st_val    := ret_data(i).st_val
        v.st_id     := ret_data(i).st_id
        v.pcq_val   := ret_data(i).pcq_val
        v.pcq_id    := ret_data(i).pcq_id
        v.paq_val   := ret_data(i).paq_val
        v.paq_id    := ret_data(i).paq_id
    }

    (io.fire_req zip ret_rdy) zip ret_fire map { case ((f, s), t) =>
        f.valid := s && state === s_ret
        f.bits := t
    }

    switch (state) {
        is (s_ret) {
            when (fence_stall) {
                state := s_ret
            } .elsewhen (ret_rdy.reduce(_|_)) {
                state := s_int_chk
            }
        }
        is (s_int_chk) {
            state := s_ret
        }
    }

    //***********************************************
    //  Exception
    val exc_cause = Wire(new RobException)
    val exc_addr = Wire(UInt(width = vaddrBits))
    val ret_has_exc = rob_done(genIndex(rob_ret_ptr)) & rob_cause(genIndex(rob_ret_ptr)).orR()

    //  Set default
    exc_cause := 0.U.asTypeOf(new RobException)
    io.sync_csr_ret := false.B
    io.sync_fence_ret := false.B
    io.csr_ret := false.B
    io.fence_ret := false.B
    io.force_write := false.B

    when (ret_has_exc && state === s_ret) {
        exc_cause.ecall := ret_data(0).inst.ecall
        exc_cause.ebreak    := ret_data(0).inst.ebreak
        exc_cause.mret      := ret_data(0).inst.mret
        exc_cause.sret      := ret_data(0).inst.sret
        exc_cause.uret      := ret_data(0).inst.uret
        exc_cause.dret      := ret_data(0).inst.dret
        exc_cause.wfi       := ret_data(0).inst.wfi
        exc_cause.sfence    := ret_data(0).inst.sfence
        exc_cause.exception := ret_cause.orR()
        exc_cause.exc_len   := ret_data(0).len
        exc_cause.exc_pc    := exc_addr
        exc_cause.exc_cause := ret_cause

        val tval_valid = isOneOf(Seq(
            Causes.breakpoint.U,
            Causes.misaligned_load.U,
            Causes.misaligned_store.U,
            Causes.load_access.U,
            Causes.store_access.U,
            Causes.fetch_access.U,
            Causes.load_page_fault.U,
            Causes.store_page_fault.U,
            Causes.fetch_page_fault.U
        ), ret_cause)
        exc_cause.badvaddr := Mux(tval_valid, exc_addr, 0.U)

        io.sync_csr_ret := ret_data(0).inst.sync_csr
        io.sync_fence_ret := ret_data(0).inst.sync_fence
        io.csr_ret := ret_data(0).inst.csr
        io.fence_ret := ret_data(0).inst.fence || ret_data(0).inst.fencei || ret_data(0).inst.sfence
        io.force_write := io.sync_fence_ret
    }

    io.retire_cause := exc_cause

    //***********************************************
    //  sfence kill
    val sfence_req = Wire(new SFenceReq)
    sfence_req := sfence_field

    io.kill.itlb_kill.valid := exc_cause.sfence || ret_data(0).inst.fencei
    io.kill.itlb_kill.bits  := Mux(ret_data(0).inst.fencei, 0.U.asTypeOf(new SFenceReq), sfence_req)
    io.kill.dtlb_kill.valid := io.kill.itlb_kill.valid
    io.kill.dtlb_kill.bits  := io.kill.itlb_kill.bits
    io.kill.mmu_kill.valid  := io.kill.itlb_kill.valid
    io.kill.mmu_kill.bits   := io.kill.itlb_kill.bits

    val itlb_kill_done = HoldControl(io.kill.itlb_kill.ready, io.kill.sfence_kill)
    val dtlb_kill_done = HoldControl(io.kill.dtlb_kill.ready, io.kill.sfence_kill)
    val mmu_kill_done  = HoldControl(io.kill.mmu_kill.ready, io.kill.sfence_kill)

    io.kill.sfence_kill := itlb_kill_done & dtlb_kill_done & mmu_kill_done

    //***********************************************
    //  Address ports
    val bjp_pos = genIndex(io.kill.bjp_rob_id)
    io.kill.bjp_pcq_id := rob_data(bjp_pos).pcq_id
    io.kill.bjp_paq_id := rob_data(bjp_pos).paq_id
    io.kill.bjp_addr_lo := rob_addr(bjp_pos)

    val addr_pos = io.exu_rob_id.map(genIndex(_))
    io.exu_addr_lo := addr_pos.map(rob_addr(_))

    io.trap_pcq_id := ret_data(0).pcq_id
    exc_addr := Cat(io.trap_addr_hi, rob_addr(genIndex(rob_ret_ptr)))
}