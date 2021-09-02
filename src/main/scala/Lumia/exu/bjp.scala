package Lumia.exu

import Chisel._
import freechips.rocketchip.util.uintToBitPat
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.DecodeLogic
import Lumia.common._
import Lumia.iq._
import Lumia.utils._

trait HasBjpCtrlSigs {
    val jal = Bool()
    val beq = Bool()
    val bne = Bool()
    val bge = Bool()
    val blt = Bool()
    val imm_sel = UInt(width = IF_SZ)
}

class BjpCtrlSigs extends Bundle with HasBjpCtrlSigs

class BjpDecoder(implicit p: Parameters) extends LumiaModule {
    val io = IO(new Bundle() {
        val uop = Input(UInt(uopBits.W))
        val sigs = Output(new BjpCtrlSigs)
    })
    val default = List[BitPat](X, X, X, X, X, IF_X)
    val insns = Array[(BitPat, List[BitPat])] (
        BitPat(uopJAL)  -> List(Y, N, N, N, N, IF_J),
        BitPat(uopJALR) -> List(Y, N, N, N, N, IF_I),
        BitPat(uopBEQ)  -> List(N, Y, N, N, N, IF_B),
        BitPat(uopBNE)  -> List(N, N, Y, N, N, IF_B),
        BitPat(uopBGE)  -> List(N, N, N, Y, N, IF_B),
        BitPat(uopBLT)  -> List(N, N, N, N, Y, IF_B))

    val decoder = DecodeLogic(io.uop, default, insns)
    val s = io.sigs
    val sigs = Seq(s.jal, s.beq, s.bne, s.bge, s.blt, s.imm_sel)
    sigs zip decoder map { case (f, s) => f := s }
}


class BjpExcUnit(implicit p: Parameters) extends ExecUnit (
    readIrf = true,
    writeIrf = true,
    dataWidth = 32,
    hasBjp = true,
    hasAddr = true
) {
    val kill = io.req.bits.kill
    //********************************
    //  Bjp decoding
    val cur_uop = io.req.bits.uop
    val bjp_decoder = Module(new BjpDecoder)
    bjp_decoder.io.uop := cur_uop.uop

    val bjp_ctrl = bjp_decoder.io.sigs
    val imm = ImmGen(io.req.bits.ip, bjp_ctrl.imm_sel)
    //********************************
    //  Extract
    val rs1_sel = cur_uop.rs1_type
    val rs2_sel = cur_uop.rs2_type
    val rd_sel  = cur_uop.rd_type
    val sign_op = cur_uop.sign_op
    //********************************
    //  Select
    val rs1 = Mux(rs1_sel === RT_REG, io.req.bits.rs1_data, 0.U /* addr */ )
    val rs2 = Mux(rs2_sel === RT_REG, io.req.bits.rs2_data, imm.asUInt())
    //********************************
    //  Calc
    val inst_addr = Cat(io.addr_hi, io.addr_lo)
    val rs2_inv = Mux(rs2(iregBits - 1), ~rs2, rs2).asUInt()
    val taddr = rs1 + rs2_inv + rs2(iregBits - 1)
    val nxt_line_addr = inst_addr + Mux(cur_uop.len, 4.U, 2.U)
    val br_com_res = Mux(bjp_ctrl.beq, rs1 === rs2
        ,   Mux(bjp_ctrl.bne, rs1 =/= rs2
            ,   Mux(bjp_ctrl.bge && sign_op, rs1.asSInt() > rs2.asSInt()
                ,   Mux(bjp_ctrl.bge && !sign_op, rs1 > rs2
                    ,   Mux(bjp_ctrl.blt && sign_op, rs1.asSInt() < rs2.asSInt(), rs1 < rs2)))))
    val br_failed_res = nxt_line_addr
    val br_res = Mux(bjp_ctrl.jal, true.B, br_com_res)
    val bjp_res = Mux(br_res, taddr, br_failed_res)
    //********************************
    //  Wakeup
    io.wakeup.valid := RegNext(io.req.valid && rd_sel === RT_REG && !kill)
    io.wakeup.bits := RegNext(cur_uop.rd)
    //********************************
    //  Write back
    io.iresp.valid := io.wakeup.valid
    io.iresp.bits.prd := io.wakeup.bits
    io.iresp.bits.data := RegNext(nxt_line_addr)
    //********************************
    //  Exec done
    io.exc_done := io.wakeup.valid
    io.cause := 0.U
    //********************************
    //  Miprediction
    val jal_tsucc = bjp_ctrl.jal && (taddr === io.taddr)
    val br_tsucc = !bjp_ctrl.jal && (((taddr === io.taddr) & br_res & io.s2_bpu_pred.taken) | (!br_res & !io.s2_bpu_pred.taken))
    val jal_pred_fail = taddr =/= io.taddr
    val br_pred_fail = ((br_res ^ io.s2_bpu_pred.taken) | (taddr =/= io.taddr && br_res && io.s2_bpu_pred.taken))
    //********************************
    //  Bjp kill
    io.bjp_kill.valid := io.req.valid && (jal_pred_fail | br_pred_fail)
    io.bjp_kill.bits.rob_id     := io.req.bits.ids.rob_id
    io.bjp_kill.bits.stq_id     := io.req.bits.ids.stq_id
    io.bjp_kill.bits.pcq_val    := io.req.bits.ids.pcq_val
    io.bjp_kill.bits.pcq_id     := io.req.bits.ids.pcq_id
    io.bjp_kill.bits.paq_val    := io.req.bits.ids.paq_val
    io.bjp_kill.bits.paq_id     := io.req.bits.ids.paq_id
    io.bjp_kill.bits.gc_tag     := io.req.bits.ids.gc_tag
    io.bjp_kill.bits.taddr      := taddr
    //********************************
    //
    val s2_pred_resp = Wire(new s2_pred_resp)
    s2_pred_resp := io.s2_bpu_pred
    s2_pred_resp := br_res
    s2_pred_resp.btac.new_br := io.s2_bpu_pred.btac.hit
    s2_pred_resp.btac.inst_type := bjp_ctrl.jal
    s2_pred_resp.btac.addr := inst_addr
    s2_pred_resp.btac.taddr := bjp_res
    s2_pred_resp.btac.len := cur_uop.len
    s2_pred_resp.btac.tsucc := jal_tsucc || br_tsucc
    io.s2_pred_resp.valid := io.req.valid
    //********************************
    //  Ready
    io.fu_types.valid := true.B && !io.bjp_kill.valid
    io.fu_types.bits := FU_BJP
}