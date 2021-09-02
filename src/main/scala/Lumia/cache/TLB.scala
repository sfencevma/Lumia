package Lumia.cache

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import Lumia.exu.Satp
import Lumia.mmu._
import Lumia.utils._

class SFenceReq(implicit p: Parameters) extends LumiaBundle {
    val rs1 = Bool()
    val rs2 = Bool()
    val vpn = UInt(width = vpnBits)
    val asid = UInt(width = asidBits)
}

class TLBEntry(implicit p: Parameters) extends LumiaBundle {
    val r       = Bool()
    val w       = Bool()
    val x       = Bool()
    val u       = Bool()
    val g       = Bool()
    val a       = Bool()
    val d       = Bool()
    val asid    = UInt(width = asidBits)
    val prv     = UInt(width = PRV.PRV_SZ)
    val vpn     = UInt(width = vpnBits)
    val ppn     = UInt(width = ppnBits)
}

class TLBReq(implicit p: Parameters) extends LumiaBundle {
    val addr = UInt(width = vaddrBits)
}

class TLBResp(implicit p: Parameters) extends LumiaBundle {
    val hit = Bool()
    val entry   = new TLBEntry
}

class TLBSet(nSets: Int)(implicit p: Parameters) extends LumiaBundle {
    val valid   = Vec(nSets, Bool())
    val entries = Vec(nSets, new TLBEntry)
    def getValid(idx: UInt) = valid(idx)
    def getG(idx: UInt)     = entries(idx).g
    def getASID(idx: UInt)  = entries(idx).asid
    def getPRV(idx: UInt)   = entries(idx).prv
    def getVPN(idx: UInt)   = entries(idx).vpn
    def getPPN(idx: UInt)   = entries(idx).ppn
    def getTag(idx: UInt)   = Cat(getG(idx), entries(idx).asid, getPRV(idx), getVPN(idx))
    def getEntry(idx: UInt) = entries(idx)
    def hit(idx: UInt, rtag: UInt) = {
        val tag = getTag(idx)
        val len = tag.getWidth
        val len_lo = PRV.PRV_SZ + vpnBits
        val len_no_g = len - 1
        getValid(idx) & (rtag === tag(len_no_g - 1, 0) | (rtag(len_lo - 1, 0) === tag(len_lo - 1, 0) & tag(len - 1)))
    }
    def refill(idx: UInt, resp: TLBEntry, enable: Bool) {
        when (enable) {
            valid(idx)         := true.B
            entries(idx).r     := resp.r
            entries(idx).w     := resp.w
            entries(idx).x     := resp.x
            entries(idx).u     := resp.u
            entries(idx).g     := resp.g
            entries(idx).a     := resp.a
            entries(idx).d     := resp.d
            entries(idx).asid  := resp.asid
            entries(idx).prv   := resp.prv
            entries(idx).vpn   := resp.vpn
            entries(idx).ppn   := resp.ppn
        }
    }
    def invalidate() = valid.foreach(_ := false.B)
    def invalidateVPN(vpn: UInt) = {
        for (i <- 0 until nSets) {
            when (getVPN(i.U) === vpn) {
                getValid(i.U) := false.B
            }
        }
    }
    def invalidateNonGlobal() = {
        for (i <- 0 until nSets) {
            when (!getG(i.U)) {
                getValid(i.U) := false.B
            }
        }
    }
}

class TLBIO(implicit p: Parameters) extends LumiaBundle {
    val satp        = new Satp().asInput// UInt(INPUT, width = satpBits)
    val prv         = UInt(INPUT, width = PRV.PRV_SZ)
    val kill        = Bool(INPUT)
    val sfence      = Valid(new SFenceReq).flip

    val req         = Decoupled(new TLBReq).flip
    val resp        = new TLBResp().asOutput
    val ptw         = new PTWIO

    val kill_done   = Bool(OUTPUT)
}


class TLB(nWays: Int, nSets: Int)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new TLBIO)
    //***********************************************
    //  TLB
    val tlb  = Reg(Vec(nWays, new TLBSet(nSets)))
    val ridx = io.req.bits.addr(11 + log2Ceil(nSets), 12)
    val rtag = Cat(io.satp.asid, io.prv, io.req.bits.addr(vaddrBits - 1, 12))
    //***********************************************
    //  Read
    val valid = tlb.map(_.getValid(ridx))
    val hits = tlb.map(_.hit(ridx, rtag))
    val rdata = tlb.map(_.getEntry(ridx))
    io.resp.hit := hits.reduce(_|_)
    io.resp.entry := Mux1H(hits, rdata)
    //***********************************************
    //  TLB finite state machine
    val s_ready::s_request::s_wait::s_wait_invalidate::Nil = Enum(4)
    val state = RegInit(s_ready)
    val reg_ridx = Reg(UInt(log2Ceil(nSets).W))
    val reg_vaddr = Reg(UInt(vaddrBits.W))
    if (usingVM) {
        val sfence = io.sfence.valid
        when(io.req.fire() && !io.resp.hit) {
            reg_ridx := ridx
            reg_vaddr := Cat(io.req.bits.addr(vaddrBits - 1, 4), UInt(0, width = 4))
            state := s_request
        }
        when(state === s_request) {
            when(sfence) { state := s_ready }
            when(io.ptw.req.ready) { state := Mux(sfence, s_wait_invalidate, s_wait) }
            when(io.kill) { state := s_ready }
        }
        when(state === s_wait && sfence) { state := s_wait_invalidate }
        when(io.ptw.resp.valid) { state := s_ready }

        //***********************************************
        //  SFence reset
        when(io.sfence.valid) {
            for (i <- 0 until nWays) {
                when(io.sfence.bits.rs1) {
                    tlb(i).invalidateVPN(io.sfence.bits.vpn)
                }.elsewhen(io.sfence.bits.rs2) {
                    tlb(i).invalidateNonGlobal()
                }.otherwise {
                    tlb(i).invalidate()
                }
            }
        }
    }
    val invalidate_refill = state === s_request || state === s_wait_invalidate
    io.kill_done := state === s_wait_invalidate || RegNext(io.sfence.valid)
    //***********************************************
    //  Update
    val repl = new PseudoLRU(nWays)
    val has_free = !valid.reduce(_&_)
    val sels = select_first(Cat(valid.map(!_).reverse), 1).map(OHToUInt(_))
    val refillWay = Mux(has_free, sels(0), repl.replace)
    val has_no_exc = !io.ptw.resp.bits.cause.orR()
    val write = io.ptw.resp.valid & !invalidate_refill & has_no_exc

    val tlbEntry = io.ptw.resp.bits.data.asTypeOf(new TLBEntry)
    for (w <- 0 until nWays) {
        tlb(w).refill(reg_ridx, tlbEntry, write && (w.U === refillWay))
    }
    //***********************************************
    //  Update hit way
    when ((io.req.valid & io.resp.hit || write) & !io.kill) {
        repl.access(Mux(io.req.valid, OHToUInt(hits), refillWay))
    }
    //***********************************************
    //
    io.req.ready := state === s_ready
    io.ptw.req.valid := state === s_request && !io.kill
    io.ptw.req.bits.addr := reg_vaddr
}