package Lumia.cache

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import Lumia.mmu._
import Lumia.utils._

case class ICacheParams (
    nSets: Int = 256,
    nWays: Int = 4,
    rowBits: Int = 4,
    nTLBSets: Int = 8,
    nTLBWays: Int = 16,
    blockBits: Int = 512
) extends L1CacheParams {
    def replacement = new PseudoLRU(nWays)
    def genIndex(pc: UInt): UInt = pc(13, 6)
    def genTag(pc: UInt): UInt = pc(33, 14)
}

trait HasL1ICacheParameters extends HasL1CacheParameters with HasCoreParameters {
    val cacheParams = tileParams.icache.get
}

class ICache(implicit p: Parameters) extends Cache with HasL1ICacheParameters {
    override val nWays: Int = cacheParams.nWays
    override val nSets: Int = cacheParams.nSets
    override val tagBits: Int = 20
    override val dataBits: Int = cacheParams.blockBits
    override def genIndex(pc: UInt): UInt = cacheParams.genIndex(pc)
    override def genTag(pc: UInt): UInt = cacheParams.genTag(pc)

}

class ICacheReq(implicit p: Parameters) extends LumiaBundle {
    val addr = UInt(width = vaddrBits)
}

class ICacheResp(implicit p: Parameters) extends LumiaBundle with HasL1ICacheParameters {
    val data = UInt(width = blockBits)
    val cause = UInt(width = xLen)
}

class ICacheStageIO(implicit p: Parameters) extends LumiaBundle with HasL1ICacheParameters {
    val kill = new Bundle() {
        val trap_kill = Bool(INPUT)
        val bjp_kill = Bool(INPUT)
        val s2_bpu_kill = Bool(INPUT)
        val iq_uc_kill = Bool(INPUT)
    }
    val prv = UInt(INPUT, width = PRV.PRV_SZ)
    val satp = UInt(INPUT, width = satpBits)
    val sfence = Valid(new SFenceReq)
    val stall = Bool(INPUT)

    val req = Valid(new ICacheReq).flip
    val resp = Valid(new ICacheResp)

    val itlb_ptw = new PTWIO
    val icache_ptw = new CachePTWIO

    val kill_done = Bool(OUTPUT)
    val icache_stall = Bool(OUTPUT)
}

class ICacheStage(implicit p: Parameters) extends LumiaModule with HasL1ICacheParameters {
    val io = IO(new ICacheStageIO)

    //*********************************************
    def getPageOffset(pc: UInt) = pc(pgIdxBits - 1, 0)
    //*********************************************
    //  Module body
    val itlb = Module(new TLB(nTLBWays, nTLBSets)).io
    val icache = Module(new ICache).io

    val kill = io.kill.elements.map(_._2.asUInt()).reduce(_|_)

    //  ITLB
    itlb.satp := io.satp
    itlb.prv := io.prv
    itlb.kill := kill
    itlb.sfence := io.sfence

    itlb.req := io.req
    //  Request a new entry
    io.itlb_ptw := itlb.ptw

    val paddr = Cat(itlb.resp.entry.ppn, UInt(0, width = 4))

    //  ICache
    icache.kill := kill
    icache.invalidate := io.sfence.valid

    icache.req.valid := io.req.valid
    icache.req.bits.addr := paddr

    //  Request a new line
    io.icache_ptw := icache.ptw

    //  Response
    val has_cause = io.itlb_ptw.resp.bits.cause.orR() || io.icache_ptw.resp.bits.cause.orR()

    val cause = Reg(UInt(width = xLen))
    val data = Reg(UInt(width = blockBits))
    when (icache.resp.hit || has_cause) {
        cause := Mux(has_cause, Mux(io.itlb_ptw.resp.valid
            , io.itlb_ptw.resp.bits.cause
            , io.icache_ptw.resp.bits.cause), 0.U)
        data := Mux(icache.resp.hit, icache.resp.data, io.icache_ptw.resp.bits.data)
    }

    val resp_val = Reg(init = Bool(false))
    resp_val := !io.icache_stall && !kill

    io.resp.valid := resp_val
    io.resp.bits.data := data
    io.resp.bits.cause := cause
    //
    io.kill_done := itlb.kill_done && icache.kill_done
    io.icache_stall := ((itlb.stall || icache.stall) & !has_cause) || io.stall
}