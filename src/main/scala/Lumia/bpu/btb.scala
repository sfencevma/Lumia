package Lumia.bpu

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import Lumia.utils._

case class BTBParams (
    nEntries: Int = 32,
    tagBits: Int = 44
) {
    def replacement = new PseudoLRU(nEntries)
}

trait HasBtbParameters extends HasCoreParameters {
    val btbParams = tileParams.btb.get
    val nEntries  = btbParams.nEntries
    val tagBits   = btbParams.tagBits
    val btbIdBits = log2Ceil(nEntries)
}

class BTBSlotUpdate(implicit p: Parameters) extends LumiaBundle with HasBtbParameters {
    val clear       = Bool()
    val br_new      = Bool()
    val modify      = Bool()
    val inst_type   = Bool()
    val addr        = UInt(width = vaddrBits)
    val taddr       = UInt(width = vaddrBits)
}

class BTBSlotResp(implicit p: Parameters) extends LumiaBundle with HasBtbParameters {
    val hit         = Bool()
    val taddr       = UInt(width = vaddrBits)
    val offset      = UInt(width = 4)
    val inst_type   = Bool()
}

class BTBSlot(implicit p: Parameters) extends LumiaModule with HasBtbParameters {
    val io = IO(new Bundle() {
        val tag     = UInt(INPUT, width = vaddrBits - 4)
        val update  = Valid(new BTBSlotUpdate).flip
        val resp    = new BTBSlotResp().asOutput
        val valid   = Bool(OUTPUT)
    })

    val valid = Reg(init = Bool(false))
    val tag = Reg(init = UInt(0, width = vaddrBits - 4))
    val taddr = Reg(init = UInt(0, width = vaddrBits))
    val offset = Reg(init = UInt(0, width = 4))
    val inst_type = Reg(init = Bool(false))

    io.valid := valid
    io.resp.hit := valid && io.tag === tag
    io.resp.taddr := taddr
    io.resp.offset := offset
    io.resp.inst_type := inst_type

    val update = io.update
    when (update.valid) {
        when (update.bits.clear) {
            valid := false.B
        } .elsewhen (update.bits.br_new) {
            valid := true.B
            tag := update.bits.addr(vaddrBits - 1, 4)
            taddr := update.bits.taddr
            offset := update.bits.addr(3, 0)
            inst_type := update.bits.inst_type
        } .elsewhen (update.bits.modify) {
            taddr := update.bits.taddr
        }
    }
}

class BTBResp(implicit p: Parameters) extends BTBSlotResp with HasBtbParameters {
    val taken = Bool()
    val addr = UInt(width = vaddrBits)
    val index = UInt(width = btbIdBits)
}

class BTBUpdate(implicit p: Parameters) extends LumiaBundle with HasBtbParameters {
    val br_new      = Bool()
    val alias_err   = Bool()
    val tsucc       = Bool()
    val inst_type   = Bool()
    val addr        = UInt(width = vaddrBits)
    val taddr       = UInt(width = vaddrBits)
    val index       = UInt(width = btbIdBits)
}

class BTB(implicit p: Parameters) extends LumiaModule with HasBtbParameters {
    val io = IO(new Bundle() {
        val req = Valid(UInt(width = vaddrBits)).flip
        val update = Valid(new BTBUpdate).flip
        val resp = new BTBSlotResp().asOutput
        val index = UInt(OUTPUT, width = btbIdBits)
    })
    def tag(addr: UInt) = addr(vaddrBits - 1, 4)
    val slots = for (s <- 0 until nEntries) yield { val slot = Module(new BTBSlot); slot }
    val btb = slots.map(_.io)
    btb.foreach(_.tag := tag(io.req.bits))

    //  Updating
    val update = io.update
    val res = Cat(update.valid, update.bits.br_new, update.bits.tsucc, update.bits.alias_err)
    val clear = res === BitPat("b10?1")
    val br_new = res === BitPat("b11??")
    val modify = res === BitPat("b1000")
    val non = res === BitPat("b1010")

    val hit = btb.map(_.resp.hit).reduce(_|_)
    //  Bypass
    val bypass_hit  = (io.req.bits(vaddrBits - 1, 4) === update.bits.addr(vaddrBits - 1, 4)) && update.valid && (br_new | modify)
    val bypass_resp = Wire(new BTBSlotResp)
    bypass_resp.hit := bypass_hit & !clear
    bypass_resp.taddr := update.bits.taddr
    bypass_resp.offset := update.bits.addr(3, 0)
    bypass_resp.inst_type := update.bits.index

    val has_free = !btb.map(_.valid).reduce(_&_)
    val sels = select_first(Cat(btb.map(_.valid).reverse), 1).map(OHToUInt(_))
    val widx = Mux(br_new, Mux(has_free, sels(0), btbParams.replacement.replace), update.bits.index)

    io.resp := Mux(bypass_hit, bypass_resp, Mux1H(btb.map(_.resp.hit), btb.map(_.resp)))
    io.index := Mux(bypass_hit, widx, OHToUInt(btb.map(_.resp.hit)))

    for (s <- 0 until nEntries) {
        btb(s).update.valid := (br_new && (s.U === widx)) || ((clear || modify) && (s.U === update.bits.index))
        btb(s).update.bits.clear := clear
        btb(s).update.bits.br_new := br_new
        btb(s).update.bits.modify := modify
        btb(s).update.bits.addr := update.bits.addr
        btb(s).update.bits.taddr := update.bits.taddr
        btb(s).update.bits.inst_type := update.bits.inst_type
    }

    //
    when ((io.req.valid & hit) || (update.valid & !non)) {
        btbParams.replacement.access(Mux(update.valid, widx, io.index))
    }
}