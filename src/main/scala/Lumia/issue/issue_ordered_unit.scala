package Lumia.issue

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._


class IssueOrderedUnit(numIssuePorts: Int
                       , numSlots: Int
                       , numWakeupPorts: Int
                       , lsu: Boolean, float: Boolean)(implicit p: Parameters) extends IssueUnit(numIssuePorts, numSlots, numWakeupPorts, float) {
    val maxShift = dispatchWidth
    val vacants = iss_slots.map(s => !s.valid) ++ dsp_reqs.map(_.valid).map(!_.asBool())
    val shamt_oh = Array.fill(numSlots + dispatchWidth){Wire(UInt(maxShift.W))}
    def SaturatingCounterOH(count_oh: UInt, inc: Bool, max: Int) = {
        val next = Wire(UInt(max.W))
        next := count_oh
        when (count_oh === 0.U && inc) {
            next := 1.U
        } .elsewhen (!count_oh(max - 1) && inc) {
            next := count_oh << 1.U
        }
        next
    }

    shamt_oh(0) := 0.U
    for (i <- 1 until numSlots + maxShift) {
        shamt_oh(i) := SaturatingCounterOH(shamt_oh(i - 1), vacants(i - 1), maxShift)
    }
    //**********************************************
    //  Module body
    //  Issue
    val uops = iss_slots.map(_.out_slot) ++ dsp_reqs
    val rs1_rdys = iss_slots.map(_.out_rs1_rdy) ++ rs1_rdy
    val rs2_rdys = iss_slots.map(_.out_rs2_rdy) ++ rs2_rdy
    val rs3_rdys = iss_slots.map(_.out_rs3_rdy) ++ rs3_rdy
    for (s <- 0 until numSlots) {
        iss_slots(s).in_slot.valid := false.B
        iss_slots(s).in_slot.bits := uops(s + 1)
        iss_slots(s).in_rs1_rdy := rs1_rdys(s + 1)
        iss_slots(s).in_rs2_rdy := rs2_rdys(s + 1)
        iss_slots(s).in_rs3_rdy := rs3_rdys(s + 1)
        for (j <- 1 to maxShift by 1) {
            when (shamt_oh(s + j) === (1 << (j - 1)).U) {
                iss_slots(s).in_slot.valid := true.B
                iss_slots(s).in_slot.bits := uops(s + j)
                iss_slots(s).in_rs1_rdy := rs1_rdys(s + j)
                iss_slots(s).in_rs2_rdy := rs2_rdys(s + j)
                iss_slots(s).in_rs3_rdy := rs3_rdys(s + j)
            }
        }
        iss_slots(s).clear := shamt_oh(s) =/= 0.U
    }
    val num_vld_entries = PopCount(iss_slots.map(_.valid))
    var num_reqs = 0.U
    for (dw <- 0 until dispatchWidth) {
        num_reqs += dsp_reqs(dw).valid
    }
    io.iss_empty := num_vld_entries < num_reqs
    io.resp.foreach(e => {
        e.valid := false.B
        e.bits := NullUop()
    })

    //  Select logic
    val requests = Cat(iss_slots.map(_.request).reverse)
    val store_req =  Array.fill(numSlots){Bool()}
    val load_req =  Array.fill(numSlots){Bool()}
    store_req(0) = true.B
    load_req(0) = true.B
    if (lsu) {
        for (s <- 0 until numSlots) {
            var load_old = false.B
            for (j <- 0 until s) {
                load_old |= iss_slots(j).cur_slot.st_val
            }
            store_req(s) = false.B
            load_req(s) = !load_old
        }
    } else {
        for (s <- 0 until numSlots) {
            store_req(s) = true.B
            load_req(s) = true.B
        }
    }

    val port_issued = Array.fill(issueWidth){Bool()}
    for (w <- 0 until issueWidth) {
        port_issued(w) = false.B
    }
    val iss_entry = Array.fill(numIssuePorts){Reg(Valid(new IssueSlotEntry))}
    for (s <- 0 until numSlots) {
        iss_slots(s).grant := false.B
        var uop_issued = false.B
        for (w <- 0 until numIssuePorts) {
            val can_allocate = (iss_slots(s).cur_slot.uop.fu === io.fu_types(w).bits) && io.fu_types(w).valid
            when (requests(w) && (store_req(s) | load_req(s)) && !uop_issued && can_allocate && !port_issued(w)) {
                iss_slots(s).grant := true.B
                io.resp(w).valid := true.B
                io.resp(w).bits := iss_slots(s).cur_slot
            }
            val was_port_issued_yet = port_issued(w)
            port_issued(w) = (requests(s) && (store_req(s) | load_req(s)) && !uop_issued && can_allocate) | port_issued(w)
            uop_issued = (requests(s) && (store_req(s) | load_req(s)) && can_allocate && !was_port_issued_yet) | uop_issued
        }
    }
    io.resp <> iss_entry
}