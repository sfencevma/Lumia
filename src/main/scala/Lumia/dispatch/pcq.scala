package Lumia.dispatch

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._

class PCQIO(implicit p: Parameters) extends LumiaBundle {
    val kill = new Bundle() {
        val trap_kill = Bool(INPUT)
        val bjp_kill = Bool(INPUT)
        val bjp_pcq_val = Bool(INPUT)
        val bjp_pcq_id = UInt(INPUT, width = paqIdBits)
    }
    val stall = Bool(INPUT)
    val alloc_req = Vec(dispatchWidth, Bool(INPUT))
    val fire_req = Vec(retireWidth, Bool(INPUT))

    val alloc_pcq_id = Vec(dispatchWidth, UInt(OUTPUT, width = pcqIdBits))
    val empty = Bool(OUTPUT)
}


class PCQ(implicit p: Parameters) extends LumiaModule {
    val io = IO(new PCQIO)
    //*******************************************
    //  Module body
    val kill = io.kill.trap_kill | (io.kill.bjp_kill & io.kill.bjp_pcq_val)
    val pcq_alloc_ptr = Reg(init = UInt(0, width = pcqIdBits))
    val pcq_fire_ptr = Reg(init = UInt(0, width = pcqIdBits))
    val alloc_nums = PopCount(io.alloc_req)
    val fire_nums = PopCount(io.fire_req)
    val allow_alloc = !io.stall
    when (kill) {
        pcq_alloc_ptr := Mux(io.kill.trap_kill, UInt(0), io.kill.bjp_pcq_id)
    } .elsewhen (allow_alloc & io.alloc_req.reduce(_|_)) {
        pcq_alloc_ptr := pcq_alloc_ptr + alloc_nums
    }
    when (io.kill.trap_kill || io.fire_req.reduce(_|_)) {
        pcq_fire_ptr := Mux(io.kill.trap_kill, UInt(0), pcq_fire_ptr + fire_nums)
    }
    val pcq_id = Wire(Vec(dispatchWidth, UInt(width = pcqIdBits)))
    pcq_id(0) := pcq_alloc_ptr
    for (dw <- 1 until dispatchWidth) {
        var nums = 0.U
        for (j <- 0 until dw) {
            nums += io.alloc_req(j)
        }
        pcq_id(dw) := pcq_alloc_ptr + nums
    }
    io.alloc_pcq_id := pcq_id
    val pcq_nums = pcq_alloc_ptr - pcq_fire_ptr
    io.empty := (numPcqEntries.U - pcq_nums) < alloc_nums
}