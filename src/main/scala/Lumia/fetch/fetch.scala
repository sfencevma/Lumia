package Lumia.fetch

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import Lumia.utils._

class FetchIO(implicit p: Parameters) extends LumiaBundle {
    val kill = new Bundle() {
        val trap_kill = Valid(UInt(width = vaddrBits)).flip
        val bjp_kill = Valid(UInt(width = vaddrBits)).flip
        val s1_bpu_kill = Valid(UInt(width = vaddrBits)).flip
        val s2_bpu_kill = Valid(UInt(width = vaddrBits)).flip
        val iq_uc_kill = Valid(UInt(width = vaddrBits)).flip
    }
    val req_block = Bool(INPUT)
    val stall = Bool(INPUT)
    val rdy_pc = Valid(UInt(width = vaddrBits))
    val nxt_block_pc = UInt(OUTPUT, width = vaddrBits)
}

class Fetch(implicit p: Parameters) extends LumiaModule {
    val io = IO(new FetchIO)
    
    val kill = io.kill.trap_kill.valid || io.kill.bjp_kill.valid || io.kill.s1_bpu_kill.valid || io.kill.s2_bpu_kill.valid || io.kill.iq_uc_kill.valid
    val core_pc = RegInit(bootAddr.U(vaddrBits.W))
    val inst_buffer = Module(new SimpleQueue(UInt(width = vaddrBits), entries = 4)).io
    val pc_need_gen = kill || inst_buffer.enq.valid
    when (pc_need_gen) {
        core_pc := MuxCase(core_pc + fetchBytes.U, Array (io.kill.trap_kill.valid   -> io.kill.trap_kill.bits
            ,   io.kill.bjp_kill.valid      -> io.kill.bjp_kill.bits
            ,   io.kill.iq_uc_kill.valid    -> io.kill.iq_uc_kill.bits
            ,   io.kill.s2_bpu_kill.valid   -> io.kill.s2_bpu_kill.bits
            ,   io.kill.s1_bpu_kill.valid   -> io.kill.s1_bpu_kill.bits
        ))
    }

    //  Buffer
    inst_buffer.reset := kill
    inst_buffer.enq.valid := !inst_buffer.full && !io.stall
    inst_buffer.enq.bits := core_pc
    inst_buffer.deq.valid := io.req_block

    //
    io.nxt_block_pc := inst_buffer.deq.bits
    io.rdy_pc.valid := pc_need_gen
    io.rdy_pc.bits := core_pc
}