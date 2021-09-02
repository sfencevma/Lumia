package Lumia.dispatch

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._

class StoreQueueIO(implicit p:Parameters) extends LumiaBundle {
    val kill = new Bundle() {
        val trap_kill = Bool(INPUT)
        val bjp_kill = Bool(INPUT)
        val bjp_stq_id = UInt(INPUT, width = stqIdBits)
    }
    val stall = Bool(INPUT)
    val exc_done = Bool(INPUT)
    val alloc_req = Vec(dispatchWidth, Bool(INPUT))
    val fire_req = Vec(dispatchWidth, Bool(INPUT))


    val alloc_stq_id = Vec(dispatchWidth, UInt(OUTPUT, width = stqIdBits))
    val stq_dsp_ptr = UInt(OUTPUT, width = stqIdBits)
    val stq_ret_ptr = UInt(OUTPUT, width = stqIdBits)
    val stq_exc_ptr = UInt(OUTPUT, width = stqIdBits)
    val empty = Bool(OUTPUT)
}

class StoreQueue(implicit p:Parameters) extends LumiaModule {
    val io = IO(new StoreQueueIO)
    val kill = io.kill.trap_kill || io.kill.bjp_kill
    val stq_alloc_ptr = RegInit(0.U(stqIdBits.W))
    val stq_fire_ptr = RegInit(0.U(stqIdBits.W))
    val stq_exc_ptr = RegInit(0.U(stqIdBits.W))
    val alloc_nums = PopCount(io.alloc_req)
    val fire_nums = PopCount(io.fire_req)
    val allow_alloc = !io.stall & !io.empty
    //  Allocation
    when (kill)                                          {
        stq_alloc_ptr := Mux(io.kill.trap_kill, UInt(0), io.kill.bjp_stq_id)
    } .elsewhen (allow_alloc & io.alloc_req.reduce(_|_)) {
        stq_alloc_ptr := stq_alloc_ptr + alloc_nums
    }
    //  Fire
    when (io.kill.trap_kill || io.fire_req.reduce(_|_)) {
        stq_fire_ptr := Mux(io.kill.trap_kill, UInt(0), stq_fire_ptr + fire_nums)
    }
    //  Execution
    when (kill) {
        stq_exc_ptr := Mux(io.kill.trap_kill, 0.U, io.kill.bjp_stq_id)
    } .elsewhen (io.exc_done) {
        stq_exc_ptr := stq_exc_ptr + 1.U
    }
    //
    val stq_id = Wire(Vec(dispatchWidth, UInt(stqIdBits.W)))
    stq_id(0) := stq_alloc_ptr
    for (dw <- 1 until dispatchWidth) {
        var nums = 0.U
        for (j <- 0 until dw) {
            nums += io.alloc_req(j)
        }
        stq_id(dw) := stq_alloc_ptr + nums
    }
    io.alloc_stq_id := stq_id
    val stq_nums = stq_alloc_ptr - stq_fire_ptr
    io.empty := (numStqEntries.U - stq_nums) < alloc_nums
    //
    io.stq_dsp_ptr := stq_alloc_ptr
    io.stq_exc_ptr := stq_exc_ptr
}