package Lumia.exu

import Chisel._
import freechips.rocketchip.util.uintToBitPat
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.{CSR, DecodeLogic}
import Lumia.common._
import Lumia.cache._
import Lumia.utils._

trait HasAluCtrlSigs {
    val add  = Bool()
    val slt  = Bool()
    val xor  = Bool()
    val or   = Bool()
    val and  = Bool()
    val sll  = Bool()
    val srl  = Bool()
    val sra  = Bool()
    val sys  = Bool()
    val csr  = Bool()
    val rw   = Bool()
    val rs   = Bool()
    val rc   = Bool()
    val sel  = UInt(IF_SZ.W)
}

class AluCtrlSigs extends Bundle with HasAluCtrlSigs


class AluDecoder(implicit p: Parameters) extends LumiaModule {
    val io = IO(new Bundle() {
        val uop = UInt(INPUT, width = uopBits.W)
        val sigs = new AluCtrlSigs().asOutput
    })

    val default = List[BitPat](X, X, X, X, X, X, X, X, X, X, X, X, X, IF_X)
    val insns = Array[(BitPat, List[BitPat])](
        BitPat(uopLUI)      -> List(N, N, N, N, N, N, N, N, N, N, N, N, N, IF_U),
        BitPat(uopAUIPC)    -> List(Y, N, N, N, N, N, N, N, N, N, N, N, N, IF_I),
        BitPat(uopADD)      -> List(Y, N, N, N, N, N, N, N, N, N, N, N, N, IF_I),
        BitPat(uopSLT)      -> List(N, Y, N, N, N, N, N, N, N, N, N, N, N, IF_I),
        BitPat(uopXOR)      -> List(N, N, Y, N, N, N, N, N, N, N, N, N, N, IF_I),
        BitPat(uopOR)       -> List(N, N, N, Y, N, N, N, N, N, N, N, N, N, IF_I),
        BitPat(uopAND)      -> List(N, N, N, N, Y, N, N, N, N, N, N, N, N, IF_I),
        BitPat(uopSLL)      -> List(N, N, N, N, N, Y, N, N, N, N, N, N, N, IF_I),
        BitPat(uopSRL)      -> List(N, N, N, N, N, N, Y, N, N, N, N, N, N, IF_I),
        BitPat(uopSRA)      -> List(N, N, N, N, N, N, N, Y, N, N, N, N, N, IF_I),
        BitPat(uopFENCE)    -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, IF_X),
        BitPat(uopFENCEI)   -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, IF_X),
        BitPat(uopVMA)      -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, IF_X),
        BitPat(uopECALL)    -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, IF_X),
        BitPat(uopEBREAK)   -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, IF_X),
        BitPat(uopWFI)      -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, IF_X),
        BitPat(uopMRET)     -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, IF_X),
        BitPat(uopSRET)     -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, IF_X),
        BitPat(uopURET)     -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, IF_X),
        BitPat(uopDRET)     -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, IF_X),
        BitPat(uopEXC)      -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, IF_X),
        BitPat(uopSYNC)     -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, IF_X),
        BitPat(uopSYNF)     -> List(N, N, N, N, N, N, N, N, Y, N, N, N, N, IF_X),
        BitPat(uopCSRRW)    -> List(N, N, N, N, N, N, N, N, N, Y, Y, N, N, IF_I),
        BitPat(uopCSRRS)    -> List(N, N, N, N, N, N, N, N, N, Y, N, Y, N, IF_I),
        BitPat(uopCSRRC)    -> List(N, N, N, N, N, N, N, N, N, Y, N, N, Y, IF_I))

    val decoder = DecodeLogic(io.uop, default, insns)
    val s = io.sigs
    val sigs = Seq(s.add, s.slt, s.xor, s.or, s.and, s.sll
        , s.srl, s.sra, s.sys, s.csr, s.rw, s.rs, s.rc, s.sel)

    sigs zip decoder map { case (f, s) => f := s }
}

class AluExecUnit(implicit p: Parameters) extends ExecUnit (
    readIrf = true,
    writeIrf = true,
    dataWidth = 32,
    hasAlu = true,
    hasCSR = true,
) {
    val kill = io.req.bits.kill
    //********************************
    //  Alu decoding
    val cur_uop = io.req.bits.uop
    val alu_decoder = Module(new AluDecoder)
    alu_decoder.io.uop := cur_uop.uop

    val alu_ctrl = alu_decoder.io.sigs
    val imm = ImmGen(io.req.bits.ip, alu_ctrl.sel)
    //********************************
    //  Extract
    val rs1_sel = cur_uop.rs1_type
    val rs2_sel = cur_uop.rs2_type
    val rs3_sel = cur_uop.rs3_type
    val rd_sel  = cur_uop.rd_type
    val sign_op = cur_uop.sign_op
    //********************************
    //  Methods
    def isSub(op: UInt) = op === uopSUB
    def Adder(op1: UInt, op2: UInt, op: Bool) = {
        val op2_inv = Mux(!op, ~op2, op2).asUInt()
        op1 + op2_inv + !op
    }
    //********************************
    //  Select
    val rs1 = Mux(rs1_sel === RT_REG, io.req.bits.rs1_data
        ,   Mux(rs1_sel === RT_PC, 0.U// io.inst_addr(0)
            ,   0.U))
    val rs2 = Mux(rs2_sel === RT_REG, io.req.bits.rs2_data
        ,   Mux(rs2_sel === RT_IMM, imm.asUInt(), 0.U))

    //********************************
    //  Adder
    val lui_res = Cat(imm(31, 12), 0.U(12.W)).asUInt()
    val op1 = Mux(cur_uop.uop === uopAUIPC, lui_res, rs1)
    val op2 = rs2
    val adder_res = Adder(op1, op2, alu_ctrl.add)
    //********************************
    //  Logic
    val slt_res = Mux(sign_op, Cat(0.U((iregBits - 1).W), rs1.asSInt() < rs2.asSInt()), Cat(0.U((iregBits - 1).W), rs1.asUInt() < rs2.asUInt()))
    //
    val shamt = rs2(4, 0)
    val shin = Mux(alu_ctrl.sll, Reverse(rs1), rs1)
    val shout_r = (Cat(sign_op & shin(iregBits - 1), shin).asSInt() >> shamt)(iregBits - 1, 0)
    val shout_l = Reverse(shout_r)
    val shift_logic_res = Mux(alu_ctrl.sll, shout_l, shout_r)
    val bit_logic_res = Mux(alu_ctrl.and, (rs1 & rs2)
        ,   Mux(alu_ctrl.or, (rs1 | rs2) , (rs1 ^ rs2)))
    //********************************
    //  csr rw
    val csr_rdata = io.csr_req
    io.csr_resp.valid := RegNext(io.req.valid && alu_ctrl.csr && !kill)
    io.csr_resp.bits.addr := RegNext(imm(CSR.ADDRSZ - 1, 0))
    val csr_wdata = Mux(alu_ctrl.rw, rs1
        ,   Mux(alu_ctrl.rs, (rs1 | csr_rdata), (~rs1 & csr_rdata)))
    io.csr_resp.bits.wdata := csr_wdata
    //********************************
    //
    val sel_logic = alu_ctrl.and | alu_ctrl.or | alu_ctrl.xor
    val sel_adder = isSub(cur_uop.uop) | alu_ctrl.add
    val sel_shift = alu_ctrl.sll | alu_ctrl.srl | alu_ctrl.sra

    val alu_res = Mux(alu_ctrl.csr, csr_rdata
        ,   Mux(alu_ctrl.slt, slt_res
            ,   Mux(sel_adder, adder_res
                ,   Mux(sel_logic, bit_logic_res
                    ,   Mux(sel_shift, shift_logic_res, lui_res)))))
    //********************************
    //  Wakeup
    io.wakeup.valid := RegNext(io.req.valid && rd_sel === RT_REG && !kill)
    io.wakeup.bits := RegNext(io.req.bits.uop.rd)
    //********************************
    //  Write back
    io.iresp.valid := io.wakeup.valid
    io.iresp.bits.prd := io.wakeup.bits
    io.iresp.bits.data := RegNext(alu_res)
    //********************************
    //  SFENCE
    val sfence = Wire(new SFenceReq)
    sfence.rs1 := cur_uop.rs1 =/= 0.U
    sfence.rs2 := cur_uop.rs2 =/= 0.U
    sfence.vpn := rs1(vaddrBits - 1, 12)
    sfence.asid := rs2(asidBits - 1, 0)
    //********************************
    //  Exec done
    io.exc_done := io.wakeup.valid || alu_ctrl.sys
    io.cause := Mux(alu_ctrl.sys, io.req.bits.cause, UInt(0))
    io.sfence := sfence
    //********************************
    //  Ready
    io.fu_types.valid := true.B
    io.fu_types.bits := FU_ALU
}
