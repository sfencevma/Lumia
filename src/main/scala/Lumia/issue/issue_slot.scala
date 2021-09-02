package Lumia.issue

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import Lumia.utils._

class IssueSlotIO(numWakeupPorts: Int)(implicit p: Parameters) extends LumiaBundle {
    val kill = new Bundle() {
        val trap_kill = Bool(INPUT)
        val bjp_kill = Bool(INPUT)
        val bjp_rob_id = UInt(INPUT, width = robIdBits)
    }
    val wakeup = Vec(numWakeupPorts, Valid(UInt(width = pregSz)).flip)
    val grant = Bool(INPUT)
    val clear = Bool(INPUT)
    val in_slot = Valid(new IssueSlotEntry).flip
    val in_rs1_rdy = Bool(INPUT)
    val in_rs2_rdy = Bool(INPUT)
    val in_rs3_rdy = Bool(INPUT)

    val valid = Bool(OUTPUT)
    val request = Bool(OUTPUT)
    val out_slot = new IssueSlotEntry().asOutput
    val out_rs1_rdy = Bool(OUTPUT)
    val out_rs2_rdy = Bool(OUTPUT)
    val out_rs3_rdy = Bool(OUTPUT)
    val cur_slot = new IssueSlotEntry().asOutput
}

class IssueSlot(numWakeupPorts: Int, float: Boolean)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new IssueSlotIO(numWakeupPorts))

    val rtype = if (float) RT_FLT else RT_REG

    val slot_uop = Reg(init = NullUop())
    val rs1_rdy = Reg(init = Bool(false))
    val rs2_rdy = Reg(init = Bool(false))
    val rs3_rdy = Reg(init = Bool(false))

    val nxt_slot = Mux(io.in_slot.valid, io.in_slot.bits, slot_uop)
    when (io.kill.trap_kill) {
        slot_uop.valid := false.B
    } .elsewhen (io.kill.bjp_kill && rob_old(io.kill.bjp_rob_id, nxt_slot.rob_id)) {
        slot_uop.valid := false.B
        slot_uop := NullUop()
    } .elsewhen (io.in_slot.valid) {
        slot_uop.valid := io.in_slot.bits.valid & !io.grant
        slot_uop := io.in_slot.bits
    } .elsewhen (io.clear) {
        slot_uop.valid := false.B
        slot_uop := NullUop()
    } .elsewhen (io.grant) {
        slot_uop.valid := false.B
    }
    when (io.in_slot.valid) {
        rs1_rdy := io.in_rs1_rdy
        rs2_rdy := io.in_rs2_rdy
        rs3_rdy := io.in_rs3_rdy
    }

    //  Wakeup
    for (i <- 0 until numWakeupPorts) {
        when(io.wakeup(i).valid && (io.wakeup(i).bits === nxt_slot.uop.rs1) && (nxt_slot.uop.rs1_type === rtype)) {
            rs1_rdy := true.B
        }
        when(io.wakeup(i).valid && (io.wakeup(i).bits === nxt_slot.uop.rs2) && (nxt_slot.uop.rs2_type === rtype)) {
            rs2_rdy := true.B
        }
        when(io.wakeup(i).valid && (io.wakeup(i).bits === nxt_slot.uop.rs3) && (nxt_slot.uop.rs3_type === rtype)) {
            rs3_rdy := true.B
        }
    }

    //  Respnse
    io.request := slot_uop.valid & rs1_rdy & rs2_rdy & rs3_rdy
    io.valid := slot_uop.valid
    io.out_slot := slot_uop
    io.out_rs1_rdy := rs1_rdy
    io.out_rs2_rdy := rs2_rdy
    io.out_rs3_rdy := rs3_rdy
    io.cur_slot := slot_uop
}