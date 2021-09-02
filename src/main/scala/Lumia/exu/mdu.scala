package Lumia.exu

import Chisel._
import freechips.rocketchip.rocket.{DecodeLogic, N, X, Y}
import freechips.rocketchip.config.Parameters
import Lumia.common._

class MulDivReq(dataBits: Int)(implicit p: Parameters) extends LumiaBundle {
    val fn = Bits(width = uopBits)
    val size = Bits(width = MEM_SZ)
    val in1 = Bits(width = dataBits)
    val in2 = Bits(width = dataBits)
}

class MulDivResp(dataBits: Int)(implicit p: Parameters) extends LumiaBundle {
    val data = Bits(width = dataBits)
}

class MulUnit(dataBits: Int)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new Bundle() {
        val req = Valid(new MulDivReq(dataBits)).flip
        val resp = Valid(new MulDivResp(dataBits))
        val busy = Bool(OUTPUT)
    })

    val in = Pipe(io.req)
    val decode = List(
        uopMUL -> List(X, X),
        uopMULH -> List(Y, Y),
        uopMULHSU -> List(Y, N),
        uopMULHU -> List(N, N)
    )
    val lhsSigned::rhsSigned::Nil = DecodeLogic(in.bits.fn, List(X, X), decode).map(_.asBool())
    val lhs = Cat(lhsSigned && in.bits.in1(dataBits - 1), in.bits.in1).asSInt()
    val rhs = Cat(rhsSigned && in.bits.in2(dataBits - 1), in.bits.in2).asSInt()
    val prod = lhs * rhs
    val muxed = Mux(in.bits.size === MEM_H, prod(2 * dataBits - 1, dataBits), prod(dataBits - 1, 0))
    val resp = Pipe(in, 1)
    io.resp.valid := resp.valid
    io.resp.bits.data := Pipe(in.valid, muxed, 1).bits

    val s_idle::s_exec::Nil = Enum(2)
    val state = RegInit(s_idle)
    val state_nxt = Wire(UInt(s_idle.getWidth.W))
    when (io.req.valid && state === s_idle) {
        state_nxt := s_exec
    } .elsewhen (resp.valid && state === s_exec) {
        state_nxt := s_idle
    } .otherwise {
        state_nxt := state
    }
    state := state_nxt
    io.busy := state =/= s_exec
}

class DivUnit(dataBits: Int)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new Bundle() {
        val req     = Valid(new MulDivReq(dataBits)).flip
        val resp    = Valid(new MulDivResp(dataBits))
        val busy    = Bool(OUTPUT)
        val kill    = Bool(INPUT)
    })
    def Adder(in1: UInt, in2: UInt, op: Bool) = {
        val in2_inv = Mux(!op, ~in2, in2).asUInt()
        in1 + in2_inv + !op
    }

    //  FSM
    private val STATE_W         = 3
    private val STATE_0TH       = 0.U(STATE_W.W)
    private val STATE_EXEC      = 1.U(STATE_W.W)
    private val STATE_REMD_CHK  = 2.U(STATE_W.W)
    private val STATE_QUOT_CORR = 3.U(STATE_W.W)
    private val STATE_REMD_CORR = 4.U(STATE_W.W)

    val state       = RegInit(STATE_0TH)
    val state_nxt   = Wire(UInt(STATE_W.W))
    val state_ena   = Wire(Bool())

    val state_0th_nxt               = Wire(UInt(STATE_W.W))
    val state_exec_nxt              = Wire(UInt(STATE_W.W))
    val state_remd_chck_nxt         = Wire(UInt(STATE_W.W))
    val state_quot_corr_nxt         = Wire(UInt(STATE_W.W))
    val state_remd_corr_nxt         = Wire(UInt(STATE_W.W))
    val state_0th_exit_ena          = Wire(Bool())
    val state_exec_exit_ena         = Wire(Bool())
    val state_remd_chck_exit_ena    = Wire(Bool())
    val state_quot_corr_exit_ena    = Wire(Bool())
    val state_remd_corr_exit_ena    = Wire(Bool())

    val special_case        = Wire(Bool())
    val state_is_0th        = state === STATE_0TH
    val state_is_exec       = state === STATE_EXEC
    val state_is_remd_chck  = state === STATE_REMD_CHK
    val state_is_quot_corr  = state === STATE_QUOT_CORR
    val state_is_remd_corr  = state === STATE_REMD_CORR

    state_0th_exit_ena := state_is_0th & !special_case & !io.kill & io.req.valid
    state_0th_nxt := STATE_EXEC

    val need_correct    = Wire(Bool())
    val last_cycle      = Wire(Bool())

    state_exec_exit_ena         := state_is_exec & (last_cycle | io.kill)
    state_exec_nxt              := Mux(io.kill, STATE_0TH, STATE_REMD_CHK)
    state_remd_chck_exit_ena    := state_is_remd_chck & (need_correct | io.kill | io.resp.valid)
    state_remd_chck_nxt         := Mux(io.kill, STATE_0TH, Mux(need_correct, STATE_QUOT_CORR, STATE_0TH))
    state_quot_corr_exit_ena    := state_is_quot_corr & (io.kill | true.B)
    state_quot_corr_nxt         := Mux(io.kill, STATE_0TH, STATE_REMD_CORR)
    state_remd_corr_exit_ena    := state_is_remd_corr & (io.kill | io.resp.valid)
    state_remd_corr_nxt         := STATE_0TH

    state_ena := state_0th_exit_ena | state_exec_exit_ena | state_remd_corr_exit_ena | state_remd_chck_exit_ena | state_quot_corr_exit_ena
    state_nxt := MuxCase(STATE_0TH, Array(state_0th_exit_ena        -> state_0th_nxt
        ,     state_exec_exit_ena       -> state_exec_nxt
        ,     state_remd_chck_exit_ena  -> state_remd_chck_nxt
        ,     state_remd_corr_exit_ena  -> state_remd_corr_nxt
        ,     state_quot_corr_exit_ena  -> state_quot_corr_nxt))
    when (state_ena) {
        state := state_nxt
    }

    //  Cycle counter
    private val CNT_W = 6
    private val CNT_1 = 1.U(CNT_W.W)
    private val CNT_32 = 32.U(CNT_W.W)

    val exec_cnt        = RegInit(0.U(CNT_W.W))
    val exec_cnt_set    = state_ena & (state_nxt === STATE_EXEC)
    val exec_cnt_inc    = state_is_exec & !last_cycle
    val exec_cnt_ena    = exec_cnt_set | exec_cnt_inc
    val exec_cnt_nxt    = Mux(exec_cnt_set, CNT_1, exec_cnt + 1.U)

    when (exec_cnt_ena) {
        exec_cnt := exec_cnt_nxt
    }

    val cycle_0th = state_is_0th
    val cycle_32nd = exec_cnt === CNT_32
    last_cycle := cycle_32nd


    val divDecode = List(
        uopDIV   -> List(Y, Y, Y),
        uopDIVU  -> List(Y, N, N),
        uopREM   -> List(N, Y, Y),
        uopREMU  -> List(N, N, N)
    )
    val cmdDIV :: lhsSigned :: rhsSigned :: Nil = DecodeLogic(io.req.bits.fn, List(X, X, X), divDecode).map(_.asBool())

    val part_quot = Reg(UInt(33.W))
    val part_remd = Reg(UInt(33.W))

    val dividend = Cat(Fill(33, lhsSigned & io.req.bits.in1(iregBits - 1)), lhsSigned & io.req.bits.in1(iregBits - 1), io.req.bits.in1)
    val divisor  = Cat(rhsSigned & io.req.bits.in2(iregBits - 1), rhsSigned & io.req.bits.in2(iregBits - 1), io.req.bits.in2)

    val quot_0cycle     = !(dividend(65) ^ divisor(33))
    val dividend_shft   = Cat(dividend(65, 0), quot_0cycle)
    val prev_quot       = Mux(cycle_0th, quot_0cycle, part_quot(0))
    val part_remd_shft  = Reg(Bool())

    val div_exec_op1    = Mux(cycle_0th, dividend_shft(66, 33), Cat(part_remd_shft, part_remd(32,0)))
    val div_exec_op2    = divisor
    val div_exec_res    = Adder(div_exec_op1, div_exec_op2, !prev_quot)

    val current_quot    = !(div_exec_res(33) ^ divisor(33))

    val div_exec_part_remd      = Cat(div_exec_res, Mux(cycle_0th, dividend_shft(32, 0), part_quot(32, 0)))
    val div_exec_part_remd_shft = Cat(div_exec_part_remd(66, 0), current_quot)
    val part_remd_ena           = Wire(Bool())
    when (part_remd_ena) {
        part_remd_shft := div_exec_res(32)
    }

    val div_exec_cnt_set    = exec_cnt_set
    val div_exec_cnt_inc    = exec_cnt_inc
    val correct_phase       = state_is_remd_corr | state_is_quot_corr
    val check_phase         = state_is_remd_chck
    val div_quot_corr_res   = Wire(UInt(34.W))
    val div_remd_corr_res   = Wire(UInt(34.W))
    val div_remd            = Mux(check_phase, part_remd(32, 0), Mux(correct_phase, div_remd_corr_res(32, 0), div_exec_part_remd(65, 33)))
    val div_quot            = Mux(check_phase | correct_phase, part_quot(32, 0), Cat(div_exec_part_remd(31, 0), 1.U(1.W)))
    val part_remd_nxt       = Mux(correct_phase, div_remd_corr_res(32, 0), Mux(state_is_exec & last_cycle, div_remd, div_exec_part_remd_shft(65, 33)))
    val part_quot_nxt       = Mux(correct_phase, div_quot_corr_res(32, 0), Mux(state_is_exec & last_cycle, div_quot, div_exec_part_remd_shft(32, 0)))

    when (part_remd_ena) {
        part_remd := part_remd_nxt
    }

    val part_quot_ena = div_exec_cnt_set | div_exec_cnt_inc | state_exec_exit_ena | state_quot_corr_exit_ena
    when (part_quot_ena) {
        part_quot := part_quot_nxt
    }

    val div_remd_chk_op1        = Cat(part_remd(32), part_remd)
    val div_remd_chk_op2        = divisor
    val div_remd_chk_res        = Adder(div_remd_chk_op1, div_remd_chk_op2, true.B)

    val remd_is_0           = !part_remd.orR()
    val remd_is_neg_div     = !div_remd_chk_res.orR()
    val remd_is_div         = part_remd === divisor(32, 0)
    need_correct := remd_is_neg_div | remd_is_div | ((part_remd(32) ^ dividend(65)) & !remd_is_0)

    val remd_inc_quot_dec = part_remd(32) ^ divisor(33)
    val div_quot_corr_op1 = Cat(part_remd(32), part_quot)
    val div_quot_corr_op2 = 1.U(34.W)
    div_quot_corr_res := Adder(div_quot_corr_op1, div_quot_corr_op2, !remd_inc_quot_dec)

    val div_remd_corr_op1 = Cat(part_remd(32), part_remd)
    val div_remd_corr_op2 = divisor
    div_remd_corr_res := Adder(div_remd_corr_op1, div_remd_corr_op2, remd_inc_quot_dec)

    part_remd_ena := div_exec_cnt_set | div_exec_cnt_inc | state_exec_exit_ena | state_remd_corr_exit_ena

    val div_res     = Mux(cmdDIV, div_quot(iregBits - 1, 0), div_remd(iregBits - 1, 0))
    val div_by_0    = !io.req.bits.in2.orR()
    val div_ovf     = io.req.bits.in2.andR() & io.req.bits.in1(iregBits - 1) & (!io.req.bits.in1(iregBits - 2, 0).orR())

    val div_by_0_res_quot   = ~0.U(iregBits.W)
    val div_by_0_res_remd   = dividend
    val div_by_0_res        = Mux(cmdDIV, div_by_0_res_quot, div_by_0_res_remd)

    val div_ovf_res_quot    = Cat(1.U(1.W), Fill(iregBits - 1, 0.U))
    val div_ovf_res_remd    = Fill(iregBits, 0.U)
    val div_ovf_res         = Mux(cmdDIV, div_ovf_res_quot, div_ovf_res_remd)

    special_case    := div_by_0 | div_ovf
    val special_res = Mux(div_by_0, div_by_0_res, div_ovf_res)

    io.resp.valid := Mux(special_case, true.B, (state_is_remd_chck & !need_correct) | state_is_remd_corr)
    io.resp.bits.data := Mux(special_case, special_res, div_res)
    //
    io.busy := state_is_exec || state_is_quot_corr || state_is_remd_corr || state_is_remd_chck
}


class MulExecUnit(implicit p: Parameters) extends ExecUnit (
    readIrf = true,
    writeIrf = true,
    dataWidth = 32,
    hasMdu = true
) {
    val kill = io.req.bits.kill
    //*****************************
    //  Mul
    val mul = Module(new MulUnit(dataBits = dataWidth)).io
    mul.req.valid := io.req.valid & !kill
    mul.req.bits.fn := io.req.bits.uop.uop
    mul.req.bits.size := io.req.bits.uop.mem_size
    mul.req.bits.in1 := io.req.bits.rs1_data
    mul.req.bits.in2 := io.req.bits.rs1_data
    io.fu_types.valid := !mul.busy
    io.fu_types.bits := FU_MUL
    //*****************************
    //  Wakeup
    io.wakeup.valid := mul.resp.valid
    io.wakeup.bits := io.req.bits.uop.rd
    //*****************************
    //  Write back
    io.iresp.valid := mul.resp.valid
    io.iresp.bits.data := mul.resp.bits.data
    io.iresp.bits.prd := io.wakeup.bits
    //*****************************
    //  Exec done
    io.exc_done := io.wakeup.valid
    io.exc_rob_id := 0.U
    io.cause := 0.U
}


class DivExecUnit(implicit p: Parameters) extends ExecUnit (
    readIrf = true,
    writeIrf = true,
    dataWidth = 32,
    hasMdu = true
) {
    //*****************************
    //
    val kill = io.req.bits.kill
    //*****************************
    //  Div
    val div = Module(new DivUnit(dataBits = iregBits)).io
    div.req.valid := io.req.valid && !kill
    div.req.bits.fn := io.req.bits.uop.uop
    div.req.bits.size := io.req.bits.uop.mem_size
    div.req.bits.in1 := io.req.bits.rs1_data
    div.req.bits.in2 := io.req.bits.rs2_data
    io.fu_types.valid := !div.busy
    io.fu_types.bits := FU_DIV
    //*****************************
    //  Wakeup
    io.wakeup.valid := div.resp.valid
    io.wakeup.bits := io.req.bits.uop.rd
    io.iresp.valid := div.resp.valid
    io.iresp.bits.data := div.resp.bits.data
    io.iresp.bits.prd := io.req.bits.uop.rd
    //*****************************
    //  Exec done
    io.exc_done := io.wakeup.valid
    io.exc_rob_id := 0.U
    io.cause := 0.U
}