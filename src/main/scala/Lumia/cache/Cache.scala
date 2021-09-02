package Lumia.cache

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import Lumia.mmu._
import Lumia.utils._

class CacheReq(implicit p: Parameters) extends LumiaBundle {
    val addr = UInt(width = paddrBits)
}

class CacheResp(dataBits: Int)(implicit p: Parameters) extends LumiaBundle {
    val hit = Bool()
    val data = UInt(width = dataBits)
}

class CacheWriteData(dataBits: Int)(implicit p: Parameters) extends LumiaBundle {
    val addr = UInt(width = paddrBits)
    val data = UInt(width = dataBits)
    val mask = UInt(width = dataBits / 8)
}

class CacheSet (
    nSets: Int,
    tagBits: Int,
    dataBits: Int
)(implicit p: Parameters) extends LumiaBundle {
    val valid = chisel3.SyncReadMem(nSets, Bool())
    val tag = chisel3.SyncReadMem(nSets, UInt(width = tagBits))
    val data = chisel3.SyncReadMem(nSets, UInt(width = dataBits))
    def getValid(idx: UInt) = valid(idx)
    def getTag(idx: UInt) = tag(idx)
    def getData(idx: UInt) = data(idx)
    def refill(idx: UInt, tag: UInt, data: UInt, enable: Bool) = {
        when (enable) {
            valid.write(idx, true.B)
            this.tag.write(idx, tag)
            this.data.write(idx, data)
        }
    }
    def invalidate() = {
        (0 until nSets) foreach(i => valid.write(i.U, false.B))
    }
}

class CacheIO(
    dataBits: Int,
    needWT: Boolean = false
)(implicit p: Parameters) extends LumiaBundle {
    val kill = Bool(INPUT)

    val req = Valid(new CacheReq).flip
    val resp = new CacheResp(dataBits).asOutput
    val ptw = new CachePTWIO
    val wt = if (needWT) DecoupledIO(new CacheWriteData(dataBits)) else null

    val invalidate = Bool(INPUT)
    val kill_done = Bool(OUTPUT)

    val stall = Bool(OUTPUT)
}

abstract class Cache(
    needWT: Boolean = false
)(implicit p: Parameters) extends LumiaModule {
    val nWays: Int
    val nSets: Int
    val tagBits: Int
    val dataBits: Int
    val io = IO(new CacheIO(dataBits, needWT))
    //***********************************************
    //
    def genIndex(pc: UInt): UInt
    def genTag(pc: UInt): UInt
    //***********************************************
    //
    val cache = Array.fill(nWays) { new CacheSet(nSets, tagBits, dataBits) }
    //  Read
    val ridx = genIndex(io.req.bits.addr)
    val rtag = genTag(io.req.bits.addr)

    val read = io.req.valid
    val valid_array = cache.map(_.getValid(ridx))
    val tag_array = cache.map(_.getTag(ridx))
    val data_array = cache.map(_.getData(ridx))

    val hits = valid_array zip tag_array map { case (v, t) => v & t === rtag }
    io.resp.hit := hits.reduce(_|_)
    io.resp.data := Mux1H(hits, data_array)
    //***********************************************
    //
    val s_ready::s_request::s_wait::s_wait_invalidate::Nil = Enum(4)
    val state = Reg(init = s_ready)
    if (usingVM) {
        val sfence = io.invalidate
        when (io.req.valid && !io.resp.hit) {
            state := s_request
        }
        when (state === s_request) {
            when (sfence) { state := s_ready }
            when (io.ptw.req.ready) { state := Mux(sfence, s_wait_invalidate, s_wait) }
            when (io.kill)  { state := s_ready }
        }
        when (state === s_wait && sfence) {
            state := s_wait_invalidate
        }
        when (io.ptw.resp.valid) { state := s_ready }
    }

    //  Invalidate
    when (io.invalidate) {
        for (c <- cache) {
            c.invalidate()
        }
    }
    val invalidate_refill = isOneOf(Seq(s_request, s_wait_invalidate), state)
    io.kill_done := state === s_wait_invalidate || RegNext(io.invalidate)

    //***********************************************
    //  Update
    val repl = new PseudoLRU(nWays)
    val has_free = !valid_array.reduce(_&_)
    val sels = select_first(Cat(valid_array.map(!_).reverse), 1).map(OHToUInt(_))
    val refillWay = Mux(has_free, sels(0), repl.replace)
    val has_no_exc = !io.ptw.resp.bits.cause.orR()
    val write = io.ptw.resp.valid & !invalidate_refill & has_no_exc

    cache.zipWithIndex.map { case (v, i) => v.refill(ridx, rtag, io.ptw.resp.bits.data, write & (i.U === refillWay)) }
    when (read & io.resp.hit || write) {
        repl.access(Mux(read, OHToUInt(hits), refillWay))
    }

    if (needWT) {

    }

    //
    io.stall := io.req.valid && !io.resp.hit && !io.ptw.resp.valid
    io.ptw.req.valid := state === s_request && !io.kill
    io.ptw.req.bits.addr := io.req.bits.addr
}
