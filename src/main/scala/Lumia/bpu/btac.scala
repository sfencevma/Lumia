package Lumia.bpu

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import Lumia.utils._

case class BTACParams (
    nSets: Int = 256,
    nWays: Int = 4,
    tagBits: Int = 42
) {
    def replacement = new PseudoLRU(nWays)
}

trait HasBtacParameters extends HasCoreParameters {
    val btacParams = tileParams.btac.get
    val nSets = btacParams.nSets
    val nWays = btacParams.nWays
    val tagBits = btacParams.tagBits
    val idBits = log2Ceil(nWays)
}

class BtacReq(implicit p: Parameters) extends LumiaBundle with HasBtacParameters {
    val addr = UInt(width = vaddrBits)
    val len  = Bool()
}

class BtacSetResp(implicit p: Parameters) extends LumiaBundle with HasBtacParameters {
    val hit         = Bool()
    val taddr       = UInt(width = vaddrBits)
    val inst_type   = Bool()
}

class BtacSetUpdate(implicit p: Parameters) extends LumiaBundle with HasBtacParameters {
    val inst_type   = Bool()
    val len         = Bool()
    val addr        = UInt(width = vaddrBits)
    val taddr       = UInt(width = vaddrBits)
}

class BtacSetIO(numReadPorts: Int)(implicit p: Parameters) extends LumiaBundle with HasBtacParameters {
    val prv    = UInt(INPUT, width = PRV.PRV_SZ)
    val req    = Vec(numReadPorts, Valid(new BtacReq).flip)
    val update = Valid(new BtacSetUpdate()).flip
    val valid  = Bool(OUTPUT)
    val resp   = Vec(numReadPorts, new BtacSetResp).asOutput
}

class BtacSet(numReadPorts: Int)(implicit p: Parameters) extends LumiaModule with HasBtacParameters {
    val io = IO(new BtacSetIO(numReadPorts))

    val valid = chisel3.SyncReadMem(nSets, Bool(false))
    val tag = chisel3.SyncReadMem(nSets, UInt(width = tagBits))
    val taddr = chisel3.SyncReadMem(nSets, UInt(width = vaddrBits))
    val inst_type = chisel3.SyncReadMem(nSets, Bool())

    //  Read
    val btac_ridx = io.req.map(_.bits.addr(log2Ceil(nSets), 1))
    val btac_rtag = io.req.map(s => Cat(s.bits.addr(vaddrBits - 1, 9), s.bits.len, io.prv))

    val rvalid = btac_ridx.map(valid(_))
    val rtag = btac_rtag.map(tag(_))

    io.resp.zipWithIndex.map { case (v, i) =>
        v.hit := btac_ridx(i) === rtag(i)
        v.taddr := taddr(btac_ridx(i))
        v.inst_type := inst_type(btac_ridx(i))
    }

    //  Write
    val widx = io.update.bits.addr(log2Ceil(nSets), 1)
    val wtag = Cat(io.update.bits.addr(vaddrBits - 1, 9), io.update.bits.len, io.prv)
    io.valid := valid(widx)
    when (io.update.valid) {
        valid.write(widx, true.B)
        tag.write(widx, wtag)
        taddr.write(widx, io.update.bits.taddr)
        inst_type.write(widx, io.update.bits.inst_type)
    }
}

class BtacResp(implicit p: Parameters) extends BtacSetResp {
    val index = UInt(width = idBits)
}

class BtacUpdate(implicit p: Parameters) extends LumiaBundle with HasBtacParameters {
    val new_br      = Bool()
    val inst_type   = Bool()
    val len         = Bool()
    val index       = UInt(width = idBits)
    val addr        = UInt(width = vaddrBits)
    val taddr       = UInt(width = vaddrBits)
    val tsucc       = Bool()
}

class BtacIO(numReadPorts: Int)(implicit p: Parameters) extends LumiaBundle {
    val prv = UInt(INPUT, width = PRV.PRV_SZ)
    val req = Vec(numReadPorts, Valid(new BtacReq).flip)
    val update = Valid(new BtacUpdate).flip
    val resp = Vec(numReadPorts, new BtacResp().asOutput)
}

class Btac(numReadPorts: Int)(implicit p: Parameters) extends LumiaModule with HasBtacParameters {
    val io = IO(new BtacIO(numReadPorts))

    //**************************************
    //  Module body
    val btac = Array.fill(nWays){ val set = Module(new BtacSet(numReadPorts)).io; set }

    //  Read
    for (way <- btac) {
        way.req := io.req
        way.prv := io.prv
    }

    val resp = btac.map(_.resp)
    val way_hits = resp.zipWithIndex.map  { case (v, i) => v(i).hit }
    val way_taddr = resp.zipWithIndex.map { case (v, i) => v(i).taddr }
    val way_type = resp.zipWithIndex.map { case (v, i) => v(i).inst_type }

    for (p <- 0 until numReadPorts) {
        io.resp(p).hit := way_hits.map(_(p)).reduce(_|_)
        io.resp(p).index := OHToUInt(way_hits(p))
        io.resp(p).taddr := Mux1H(way_hits(p), way_taddr(p))
        io.resp(p).inst_type := Mux1H(way_hits(p), way_type(p))
    }

    //  Write
    val repl = new PseudoLRU(nWays)
    val has_free = btac.map(!_.valid).reduce(_|_)
    val free_way = select_first(Cat(btac.map(!_.valid).reverse), 1).map(OHToUInt(_))
    val widx = Mux(has_free, free_way(0), repl.replace)

    for (w <- 0 until nWays) {
        btac(w).update.valid := !io.update.bits.tsucc && Mux(io.update.bits.new_br, w.U === widx, w.U === io.update.bits.index) && io.update.valid
        btac(w).update.bits.inst_type := io.update.bits.inst_type
        btac(w).update.bits.len := io.update.bits.len
        btac(w).update.bits.addr := io.update.bits.addr
        btac(w).update.bits.taddr := io.update.bits.taddr
    }
}