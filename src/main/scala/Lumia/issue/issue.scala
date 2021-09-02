package Lumia.issue

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.dispatch._
import Lumia.common._
import Lumia.utils._

case class IssueParams (
     issueWidth: Int = 2,
     numEntries: Int = 16,
     lsu: Boolean = false,
     float: Boolean = false
)

object IssueConsts {
    val ALU_PORT = 0
    val BJP_PORT = 1
    val MUL_PORT = 2
    val DIV_PORT = 3
    val LSU_PORT = 4
    val FPU_PORT = 5
}

class IssueCtrl(implicit p: Parameters) extends LumiaModule {
    val io = IO(new Bundle() {
        val kill = new Bundle() {
            val trap_kill = Bool(INPUT)
            val bjp_kill = Bool(INPUT)
            val bjp_rob_id = UInt(INPUT, width = robIdBits)
        }
        val req = Vec(issueWidth, Valid(new IssueSlotEntry).flip)
        val resp = Vec(issueWidth, Valid(new IssueSlotEntry))
        val fire = Vec(issueWidth, Bool(INPUT))
    })

    val valid = Reg(Vec(issueWidth, Bool(false)))
    val uops = Reg(Vec(issueWidth, new IssueSlotEntry))

    val iss_kills = io.req.map (s => s.valid && (io.kill.trap_kill || (io.kill.bjp_kill && rob_old(io.kill.bjp_rob_id, s.bits.rob_id))))
    val exec_kills = valid zip uops map { case (f, s) => io.kill.trap_kill || (f && io.kill.bjp_kill && rob_old(io.kill.bjp_rob_id, s.rob_id)) }

    for (iw <- 0 until issueWidth) {
        //  Fire
        when (io.fire(iw)) {
            valid(iw) := false.B
        }
        //  Kill
        when (exec_kills(iw)) {
            valid(iw) := false.B
        }
        //  Issue
        when (!iss_kills(iw) & io.req(iw).valid) {
            valid(iw) := true.B
            uops(iw) := io.req(iw).bits
        }
    }

    io.resp.zipWithIndex.map { case (v, i) =>
        v.valid := valid(i)
        v.bits := uops(i)
    }
}

class IssueIO(implicit p: Parameters) extends LumiaBundle {
    val kill = new Bundle() {
        val trap_kill = Bool(INPUT)
        val bjp_kill = Bool(INPUT)
        val bjp_rob_id = UInt(INPUT, width = robIdBits)
    }
    val req = Vec(dispatchWidth, Valid(new DispatchResp).flip)
    val wakeup = Vec(issueWidth, Valid(UInt(width = pregSz)).flip)
    val fu_types = Vec(issueWidth, Valid(UInt(width = FU_SZ)).flip)

    val stall = Bool(OUTPUT)
    val resp = Vec(issueWidth, Valid(new IssueSlotEntry).flip)
}


class Issue(implicit p: Parameters) extends LumiaModule {
    val io = IO(new IssueIO)
    val IssueQueue = issueParams.map(s => {
        val iu = Module(new IssueOrderedUnit(s.issueWidth, s.numEntries, issueWidth, s.lsu, s.float)).io
        iu.kill.trap_kill := io.kill.trap_kill
        iu.kill.bjp_kill := io.kill.bjp_kill
        iu.kill.bjp_rob_id := io.kill.bjp_rob_id
        iu.wakeup := io.wakeup
        iu.req := io.req
        iu
    })
    IssueQueue.map(_.fu_types).flatten zip io.fu_types map { case (f, s) => f := s }

    //  Issue Control
    val iss_ctrl = Module(new IssueCtrl)
    iss_ctrl.io.kill := io.kill
    iss_ctrl.io.req := IssueQueue.map(_.resp).flatten
    io.resp := iss_ctrl.io.resp
    iss_ctrl.io.fire := io.wakeup.map(_.valid)

    //
    io.stall := IssueQueue.map(_.iss_empty).reduce(_|_)
}