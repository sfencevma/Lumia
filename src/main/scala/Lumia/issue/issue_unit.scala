package Lumia.issue

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.dispatch._
import Lumia.common._
import Lumia.utils._

class IssueUnitIO(numIssuePorts: Int, numWakeupPorts: Int)(implicit p: Parameters) extends LumiaBundle {
    val kill = new Bundle() {
        val trap_kill = Bool(INPUT)
        val bjp_kill = Bool(INPUT)
        val bjp_rob_id = UInt(INPUT, width = robIdBits)
    }
    val req = Vec(dispatchWidth, Valid(new DispatchResp).flip)
    val wakeup = Vec(numWakeupPorts, Valid(UInt(width = pregSz)).flip)
    val fu_types = Vec(numIssuePorts, Valid(UInt(width = FU_SZ)).flip)

    val iss_empty = Bool(OUTPUT)
    val resp = Vec(numIssuePorts, Valid(new IssueSlotEntry))
}


abstract class IssueUnit(numIssuePorts: Int, numSlots: Int, numWakeupPorts: Int, float: Boolean)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new IssueUnitIO(numIssuePorts, numWakeupPorts))
    //*********************************************
    //  Module body
    val slots = for (s <- 0 until numSlots) yield {val slot = Module(new IssueSlot(numWakeupPorts, float)); slot}
    val iss_slots = Vec(slots.map(_.io))
    val dsp_reqs = Array.fill(dispatchWidth){Wire(new IssueSlotEntry)}
    for (dw <- 0 until dispatchWidth) {
        dsp_reqs(dw).valid := io.req(dw).valid
        dsp_reqs(dw).rob_id := io.req(dw).bits.rob_id
        dsp_reqs(dw).st_val := io.req(dw).bits.st_val
        dsp_reqs(dw).stq_id := io.req(dw).bits.stq_id
        dsp_reqs(dw).s2_bpu_pred := io.req(dw).bits.s2_bpu_pred
        dsp_reqs(dw).uop := io.req(dw).bits.uop
    }

    val rs1_rdy = Array.fill(dispatchWidth){Wire(Bool())}
    val rs2_rdy = Array.fill(dispatchWidth){Wire(Bool())}
    val rs3_rdy = Array.fill(dispatchWidth){Wire(Bool())}
    for (dw <- 0 until dispatchWidth) {
        rs1_rdy(dw) := dsp_reqs(dw).uop.rs1_type =/= RT_REG || dsp_reqs(dw).uop.rs1_type =/= RT_FLT
        rs2_rdy(dw) := dsp_reqs(dw).uop.rs2_type =/= RT_REG || dsp_reqs(dw).uop.rs2_type =/= RT_FLT
        rs3_rdy(dw) := dsp_reqs(dw).uop.rs3_type =/= RT_REG || dsp_reqs(dw).uop.rs3_type =/= RT_FLT
    }

    for (slot <- iss_slots) {
        slot.kill.trap_kill := io.kill.trap_kill
        slot.kill.bjp_kill := io.kill.bjp_kill
        slot.kill.bjp_rob_id := io.kill.bjp_rob_id
        slot.wakeup := io.wakeup
    }
}