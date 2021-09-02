package Lumia.mmu

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import Lumia.cache._
import Lumia.utils._

case class PTECacheParams (
    nSets: Int = 512,
    nWays: Int = 4
) {
    def replacement = new PseudoLRU(nWays)
    def genIndex(pc: UInt): UInt = pc(20, 12)
    def genTag(satp: UInt, prv: UInt, pc: UInt): UInt = Cat(satp(30, 22), prv, pc(31, 12))
}

trait HasPTECacheParameters extends HasTileParameters with HasCoreParameters {
    val pteCacheParams = tileParams.ptecache.get
    val nWays = pteCacheParams.nWays
    val nSets = pteCacheParams.nSets
}

class PTECacheEntry(implicit p: Parameters) extends LumiaBundle {
    val pte     = new PTE
    val vpn     = UInt(width = vpnBits)
    val level   = UInt(width = 2)
    val prv     = UInt(width = PRV.PRV_SZ)
    val asid    = UInt(width = asidBits)
}

class PTECacheReq(implicit p: Parameters) extends LumiaBundle {
    val addr = UInt(width = vaddrBits)
}

class PTECacheResp(implicit p: Parameters) extends LumiaBundle {
    val hit = Bool()
    val entry = new PTECacheEntry
}

class PTECacheSet(nSets: Int)(implicit p: Parameters) extends LumiaBundle {
    val valid = Vec(nSets, Bool(false))
    val entries = Vec(nSets, new PTECacheEntry)
    //**********************************************
    //  Methods
    def getValid(idx: UInt)     = valid(idx)
    def getASID(idx: UInt)      = entries(idx).asid
    def getPRV(idx: UInt)       = entries(idx).prv
    def getVPN(idx: UInt)       = entries(idx).vpn
    def getLevel(idx: UInt)     = entries(idx).level
    def getPTE(idx: UInt)       = entries(idx).pte
    def getG(idx: UInt)         = getPTE(idx).g
    def getTag(idx: UInt)       = Cat(getG(idx), entries(idx).asid, getPRV(idx), getVPN(idx))
    def getEntry(idx: UInt)     = entries(idx)
    def hit(idx: UInt, rtag: UInt) = {
        val tag = getTag(idx)
        val len = tag.getWidth
        val len_lo = PRV.PRV_SZ + vpnBits
        val len_no_g = len - 1
        getValid(idx) & (rtag === tag(len_no_g - 1, 0) | (rtag(len_lo - 1, 0) === tag(len_lo - 1, 0) & tag(len - 1)))
    }
    def refill(idx: UInt, resp: PTECacheEntry, enable: Bool) {
        when (enable) {
            valid(idx)         := true.B
            entries(idx).asid  := resp.asid
            entries(idx).prv   := resp.prv
            entries(idx).vpn   := resp.vpn
            entries(idx).level := resp.level
            entries(idx).pte   := resp.pte
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

class PTECacheIO(implicit p: Parameters) extends LumiaBundle {
    val satp        = UInt(INPUT, width = satpBits)
    val prv         = UInt(INPUT, width = PRV.PRV_SZ)
    val kill        = Bool(INPUT)
    val sfence      = Valid(new SFenceReq).flip

    val req         = Decoupled(new PTECacheReq).flip
    val resp        = new PTECacheResp().asOutput
    val ptw         = new PTWIO((new PTE).getWidth)

    val kill_done   = Bool(OUTPUT)
}

class PTECache(implicit p: Parameters) extends LumiaModule with HasPTECacheParameters {
    val io = IO(new PTECacheIO)

    //**********************************************
    //  PTE Cache
    val pte_cache = Reg(Vec(nWays, new PTECacheSet(nSets)))
    //**********************************************
    //  Read
    val ridx = pteCacheParams.genIndex(io.req.bits.addr)
    val rtag = pteCacheParams.genTag(io.satp, io.prv, io.req.bits.addr)

    val valid = pte_cache.map(_.getValid(ridx))
    val hits = pte_cache.map(_.hit(ridx, rtag))
    val rdata = pte_cache.map(_.getEntry(ridx))

    io.resp.hit := hits.reduce(_|_)
    io.resp.entry := Mux1H(hits, rdata)

    //**********************************************
    //  PTE Cache finite state machine
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
            for (w <- 0 until nWays) {
                when(io.sfence.bits.rs1) {
                    pte_cache(w).invalidateVPN(io.sfence.bits.vpn)
                }.elsewhen(io.sfence.bits.rs2) {
                    pte_cache(w).invalidateNonGlobal()
                }.otherwise {
                    pte_cache(w).invalidate()
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

    val pteCacheEntry = io.ptw.resp.bits.data.asTypeOf(new PTECacheEntry)
    for (w <- 0 until nWays) {
        pte_cache(w).refill(reg_ridx, pteCacheEntry, write && (w.U === refillWay))
    }
    //***********************************************
    //  Update hit way
    when ((io.req.valid & io.resp.hit || write) & !io.kill) {
        repl.access(Mux(io.req.valid, OHToUInt(hits), refillWay))
    }
    //***********************************************
    //
    io.req.ready := io.req.valid && !io.resp.hit
    io.ptw.req.valid := state === s_request && !io.kill
    io.ptw.req.bits.addr := reg_vaddr
}