package Lumia.decode

import Chisel._
import freechips.rocketchip.rocket.Causes
import freechips.rocketchip.config.Parameters
import Lumia.iq._
import Lumia.common._

class DecodeResp(implicit p: Parameters) extends LumiaBundle {
    val addr            = UInt(width = vaddrBits)
    val taddr           = UInt(width = vaddrBits)
    val len             = Bool()
    val cause           = UInt(width = xLen)
    val s2_bpu_pred     = new s2_bpu_pred
    val uop             = new Uop
}

class DecodeIO(implicit p: Parameters) extends LumiaBundle {
    val kill = new Bundle() {
        val trap_kill = Bool(INPUT)
        val bjp_kill = Bool(INPUT)
    }
    val stall = Bool(INPUT)
    val req = Vec(decodeWidth, Valid(new IqResp).flip)

    val resp = Vec(decodeWidth, Valid(new DecodeResp))
    val dec_stall = Bool(OUTPUT)
}


class Decode(implicit p: Parameters) extends LumiaModule {
    val io = IO(new DecodeIO)

    //*******************************************
    //  Module body
    val kill = io.kill.trap_kill | io.kill.bjp_kill
    val decoders = Seq.fill(decodeWidth)(Module(new Decoder).io)
    decoders zip  io.req map {case (f, s) => f.inst := s.bits.inst}

    val exc_uop = spec_uop(uopEXC)
    val has_exc = io.req.map(_.bits.cause.orR())
    val addrs = io.req.map(_.bits.addr)
    val taddrs = io.req.map(_.bits.inst_taddr)
    val lens = io.req.map(_.bits.len)
    val ilgls = decoders.map(_.ilgl)
    val uops = decoders.map(_.uop)

    val out = Array.fill(decodeWidth){Reg(Valid(new DecodeResp))}
    for (dw <- 0 until decodeWidth) {
        when(kill) {
            out(dw).valid := false.B
        }.elsewhen(io.req(dw).valid & !io.stall) {
            out(dw).valid := true.B
            out(dw).bits.uop := Mux(ilgls(dw) | has_exc(dw), exc_uop, uops(dw))
            out(dw).bits.addr := addrs(dw)
            out(dw).bits.taddr := taddrs(dw)
            out(dw).bits.len := lens(dw)
            out(dw).bits.cause := Mux(has_exc(dw), io.req(dw).bits.cause
                , Mux(ilgls(dw), Causes.illegal_instruction.U, 0.U(xLen.W)))
            out(dw).bits.s2_bpu_pred := io.req(dw).bits.s2_bpu_pred
        } .otherwise {
            out(dw).valid := false.B
        }
    }
    io.resp := out
    io.dec_stall := io.stall
}
