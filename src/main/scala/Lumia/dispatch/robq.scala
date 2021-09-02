package Lumia.dispatch

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._

class RobQueueIO(implicit p: Parameters) extends LumiaBundle {
    val kill = new Bundle() {
        val trap_kill = Bool(INPUT)
        val bjp_kill = Bool(INPUT)
        val bjp_rob_id = UInt(INPUT, width = robIdBits)
    }
    val stall = Bool(INPUT)
    val alloc_req = Vec(dispatchWidth, Bool(INPUT))
    val fire_req = Vec(dispatchWidth, Bool(INPUT))

    val alloc_rob_id = Vec(dispatchWidth, UInt(OUTPUT, width = robIdBits))
    val dsp_ptr = UInt(OUTPUT, width = robIdBits)
    val ret_ptr = UInt(OUTPUT, width = robIdBits)
    val empty = Bool(OUTPUT)
}


class RobQueue(implicit p: Parameters) extends LumiaModule {
    val io = IO(new RobQueueIO)
    //******************************************
    //  Module body
    val kill = io.kill.trap_kill || io.kill.bjp_kill
    val robq_alloc_ptr = Reg(init = UInt(0, width = robIdBits))
    val robq_fire_ptr = Reg(init = UInt(0, width = robIdBits))
    val alloc_nums = PopCount(io.alloc_req)
    val fire_nums = PopCount(io.fire_req)
    val allow_alloc = !io.stall && !io.empty
    when (kill) {
        robq_alloc_ptr := Mux(io.kill.trap_kill, UInt(0), io.kill.bjp_kill)
    } .elsewhen (allow_alloc && io.alloc_req.reduce(_|_)) {
        robq_alloc_ptr := robq_alloc_ptr + alloc_nums
    }
    when (io.fire_req.reduce(_|_) || io.kill.trap_kill) {
        robq_fire_ptr := Mux(io.kill.trap_kill, 0.U(robIdBits.W), robq_fire_ptr + fire_nums)
    }
    for (dw <- 0 until dispatchWidth) {
        io.alloc_rob_id(dw) := robq_alloc_ptr + dw.U
    }
    val robq_nums = robq_alloc_ptr - robq_fire_ptr
    io.dsp_ptr := robq_alloc_ptr
    io.ret_ptr := robq_fire_ptr
    io.empty := (numRobEntries.U - robq_nums) < alloc_nums
}
