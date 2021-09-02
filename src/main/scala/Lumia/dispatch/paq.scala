package Lumia.dispatch

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._

class PAQIO(implicit p: Parameters) extends LumiaBundle {
    val kill = new Bundle() {
        val trap_kill = Bool(INPUT)
        val bjp_kill = Bool(INPUT)
        val bjp_paq_val = Bool(INPUT)
        val bjp_paq_id = UInt(INPUT, width = paqIdBits)
    }
    val stall = Bool(INPUT)
    val alloc_req = Vec(dispatchWidth, Bool(INPUT))
    val fire_req = Vec(retireWidth, Bool(INPUT))

    val alloc_paq_id = Vec(dispatchWidth, UInt(INPUT, width = pcqIdBits))
    val empty = Bool(INPUT)
}


class PAQ(implicit p: Parameters) extends LumiaModule {
    val io = IO(new PAQIO)

    //**********************************************
    //  Module body
    val kill = io.kill.trap_kill | (io.kill.bjp_kill & io.kill.bjp_paq_val)
    val paq_alloc_ptr = Reg(init = UInt(0, width = paqIdBits))
    val paq_fire_ptr = Reg(init = UInt(0, width = paqIdBits))
    val alloc_nums = PopCount(io.alloc_req)
    val fire_nums = PopCount(io.fire_req)
    val allow_alloc = !io.stall
    when (kill) {
        paq_alloc_ptr := Mux(io.kill.trap_kill, 0.U, io.kill.bjp_paq_id)
    } .elsewhen (allow_alloc & io.alloc_req.reduce(_|_)) {
        paq_alloc_ptr := paq_alloc_ptr + alloc_nums
    }
    when (io.kill.trap_kill || io.fire_req.reduce(_|_)) {
        paq_fire_ptr := Mux(io.kill.trap_kill, 0.U, paq_fire_ptr + fire_nums)
    }
    val paq_id = Wire(Vec(dispatchWidth, UInt(pcqIdBits.W)))
    paq_id(0) := paq_alloc_ptr
    for (dw <- 1 until dispatchWidth) {
        var nums = 0.U
        for (j <- 0 until dw) {
            nums += io.alloc_req(j)
        }
        paq_id(dw) := paq_alloc_ptr + nums
    }
    io.alloc_paq_id := paq_id
    val paq_nums = paq_alloc_ptr - paq_fire_ptr
    io.empty := (numPcqEntries.U - paq_nums) < alloc_nums
}