package Lumia.predecoder

import Chisel._

import freechips.rocketchip.config.Parameters

import Lumia.common._
import Lumia.cache._
import Lumia.bpu._
import Lumia.utils._

class PreDecoderResp(implicit p: Parameters) extends LumiaBundle {
    val inst = UInt(width = instBits)
    val addr = UInt(width = vaddrBits)
    val len = Bool()
    val inst_type = UInt(width = 6)
    val alias_vec = UInt(width = 4)
    val match_vec = UInt(width = 4)
}

class PreDecoderIO(implicit p: Parameters) extends LumiaBundle {
    val kill = new Bundle() {
        val trap_kill = Bool(INPUT)
        val bjp_kill = Bool(INPUT)
    }
    val s1_pred_resp =Valid(new s1_pred_resp).flip
    val cache_req = Valid(new ICacheResp).flip
    val bypass_req = Valid(new ICacheResp).flip
    val s1_bpu_pred = new s1_bpu_pred().asInput
    val fetch_pc = UInt(OUTPUT, width = vaddrBits)
    val stall = Bool(INPUT)
    val predec_resp = Vec(decodeWidth, Valid(new PreDecoderResp))
    val iq_bpu_pred =new s1_bpu_pred().asOutput
    val exc_cause = UInt(OUTPUT, width = xLen)
    val block_done = Bool(OUTPUT)
    val line_done = Bool(OUTPUT)
}

class PreDecoder(implicit p: Parameters) extends LumiaModule {
    val io = IO(new PreDecoderIO)

    private val blockSize = 16 * 8
    private val decodeSize = 2 * 8
    private val totalInsts = 8

    def inc_ctr(ctr: SInt, taken: Bool, step: Int) = new Predictor().inc_ctr(ctr, taken, step)
    def is4Bytes(bytes: UInt) = bytes(1, 0) === 3.U(2.W)
    def is2Bytes(bytes: UInt) = !is4Bytes(bytes)
    def extract(bytes: UInt, elemSz: Int) = {
        var bytes_array = Array[UInt]()
        for (i <- 0 until bytes.getWidth / elemSz) {
            bytes_array = bytes_array :+ bytes((i + 1) * elemSz - 1, i * elemSz)
        }
        bytes_array
    }
    def genMask(len: Int, offset: UInt) = (Fill(len, 1.U) << offset)(len - 1, 0)
    def genEndPosition(bytes: Seq[UInt], offset: UInt) = {
        var endPos = 0.U((2 * bytes.length).W)
        var _4 = false.B
        for (i <- 1 until 2 * bytes.length by 2) {
            endPos = endPos.bitSet(i.U, !(i.U < offset) && Mux(_4, true.B, !is4Bytes(bytes(i / 2))))
            _4 = Mux(_4, false.B, is4Bytes(bytes(i / 2)))
        }
        endPos(2 * bytes.length - 1, 0) & genMask(2 * bytes.length, offset)
    }

    //*************************************
    //  Module body
    val kill = io.kill.trap_kill | io.kill.bjp_kill
    val predec_req = (io.cache_req.valid | io.bypass_req.valid) & !kill
    val block_array = extract(Mux(io.cache_req.valid, io.cache_req.bits.data, io.bypass_req.bits.data), blockSize)
    val instBuffer = Reg(Vec(3, UInt(blockSize.W)))
    for (b <- 0 until 3) {
        when (predec_req) {
            instBuffer(b) := block_array(b + 1)
        }
    }

    val fetch_nxt_block = Wire(Bool())
    val c_block_array   = block_array zip (block_array(0) +: instBuffer) map {case (f, s) => Mux(predec_req, f, s)}
    val sel_block       = Mux1H(UIntToOH(io.fetch_pc(5, 4)), c_block_array)
    val left_buffer     = RegInit(0.U(decodeSize.W))
    when (predec_req | fetch_nxt_block) {
        left_buffer := sel_block(sel_block.getWidth - 1, sel_block.getWidth - decodeSize)
    }

    val bytes_array     = extract(sel_block, decodeSize)
    val endPos          = genEndPosition(bytes_array, io.fetch_pc(3, 0))
    val cross           = RegInit(false.B)
    val insts_valid     = Wire(Vec(totalInsts, Bool()))
    val insts           = Wire(Vec(totalInsts, UInt(instBits.W)))
    val insts_addr      = Wire(Vec(totalInsts, UInt(vaddrBits.W)))
    val insts_len       = Wire(Vec(totalInsts, Bool()))
    val insts_alias     = Wire(Vec(totalInsts, UInt(4.W)))
    val insts_match     = Wire(Vec(totalInsts, UInt(4.W)))
    val insts_type      = Wire(Vec(totalInsts, UInt(6.W)))
    val need_2_cycles   = Reg(Bool())

    //  Control
    val s_idle::s_cycle_1::s_cycle_2::Nil = Enum(3)
    val state           = RegInit(s_idle)
    val state_nxt       = Wire(UInt(s_idle.getWidth.W))
    val state_is_idle   = state === s_idle
    val state_is_cycle_1= state === s_cycle_1
    val state_is_cycle_2= state === s_cycle_2
    val last_cycle      = (!need_2_cycles & state_is_cycle_1) || state_is_cycle_2
    when (!kill) {
        when (!io.stall) {
            when (predec_req | fetch_nxt_block) {
                state_nxt := s_cycle_1
            } .elsewhen (state_is_cycle_1 && need_2_cycles) {
                state_nxt := s_cycle_2
            } .otherwise {
                state_nxt := s_idle
            }
        } .otherwise {
            state_nxt := state
        }
    } .otherwise {
        state_nxt := s_idle
    }
    state := state_nxt
    fetch_nxt_block := last_cycle & !io.stall
    when (kill) {
        cross := false.B
    } .elsewhen (io.block_done) {
        cross := Mux(need_2_cycles, RegNext(!endPos(endPos.getWidth - 1)), !endPos(endPos.getWidth - 1))
    }

    //  Extracting
    for (b <- 1 until endPos.getWidth by 2) {
        //  Extracting instructions
        when (predec_req | fetch_nxt_block) {
            when (endPos(b)) {
                when (b.U === 1.U) {
                    insts(b / 2)        := Mux(cross, Cat(bytes_array(b / 2), left_buffer), Cat(0.U(16.W), bytes_array(b / 2)))
                    insts_len(b / 2)    := cross
                } .otherwise {
                    when (is4Bytes(bytes_array((b - 2) / 2))) {
                        insts(b / 2)        := Cat(bytes_array(b / 2), bytes_array((b - 2) / 2))
                        insts_len(b / 2)    := true.B
                    } .otherwise {
                        insts(b / 2)        := Cat(0.U(16.W), bytes_array(b / 2))
                        insts_len(b / 2)    := false.B
                    }
                }
            } .otherwise {
                insts(b / 2)        := 0.U
                insts_len(b / 2)    := false.B
            }
        } .otherwise {
            insts(b / 2)        := 0.U
            insts_len(b / 2)    := false.B
        }

        when (predec_req | fetch_nxt_block) {
            when (endPos(b)) {
                insts_valid(b / 2) := true.B
            } .otherwise {
                insts_valid(b / 2) := false.B
            }
        } .otherwise {
            insts_valid(b / 2) := false.B
        }
    }

    //  Select instructions
    val sels        = select_first(insts_valid.asUInt(), insts_valid.length).map(OHToUInt(_))
    val sel_insts   = Reg(Vec(totalInsts, new PreDecoderResp))
    val numInsts    = PopCount(insts_valid)
    insts_addr(0) := Mux(cross, io.fetch_pc - 2.U, io.fetch_pc)
    for (i <- 1 until totalInsts) {
        insts_addr(i) := insts_addr(i - 1) + Mux(insts_len(i), 4.U, 2.U)
    }
    //  Generate check vectors
    val alias_check = UIntToOH(io.s1_bpu_pred.btb.offset) & ~endPos
    val match_check = UIntToOH(io.s1_bpu_pred.btb.offset) & endPos
    for (i <- 0 until totalInsts) {
        insts_alias(i) := Mux(insts_len(i), Cat(alias_check(i * 2 + 1, i * 2), 0.U(2.W)), Cat(0.U(2.W), alias_check(i * 2 + 1, i * 2)))
        insts_match(i) := Mux(insts_len(i), Cat(match_check(i * 2 + 1, i * 2), 0.U(2.W)), Cat(0.U(2.W), match_check(i * 2 + 1, i * 2)))
    }
    //  Pre-decode
    for (i <- 0 until totalInsts) {
        insts_type(i)   := Cat(
            insts(i)(6, 0) === "b1110011".U(7.W) & (insts(i)(14, 12) =/= "b000".U(3.W) | insts(i)(14, 12) =/= "b100".U)
            ,   (insts(i)(31, 25) === "b0001001".U(7.W) & insts(i)(14, 0) === "b000000001110011".U(15.W)) | (insts(i)(14, 13) === "b00".U(2.W) & insts(i)(6, 0) === "b0001111".U(7.W))
            ,   insts(i)(6, 0) === "b1101111".U(7.W) | (insts(i)(6, 0) === "b1100111".U(7.W) & insts(i)(14, 12) === "b000".U(3.W) & insts(i)(11, 7) =/= "b00000".U(5.W))
            ,   (insts(i)(1, 0) === "b01".U(2.W) & insts(i)(14, 13) === "b01".U(2.W)) | (insts(i)(6, 0) === "b0000010".U(7.W) & insts(i)(15, 13) === "b100".U(3.W) & insts(i)(11, 7) =/= "b00000".U(5.W))
            ,   insts(i)(6, 0) === "b1100011".U(7.W) & insts(i)(14, 12) =/= "b010".U(3.W) & insts(i)(14, 12) =/= "b011".U(3.W)
            ,   insts(i)(1, 0) === "b01".U(2.W) & insts(i)(15, 14) === "b11".U(2.W)
        )
    }

    when (predec_req | fetch_nxt_block) {
        for (i <- 0 until sels.length) {
            sel_insts(i).inst       := insts(sels(i))
            sel_insts(i).addr  := insts_addr(sels(i))
            sel_insts(i).len   := insts_len(sels(i))
            sel_insts(i).alias_vec  := insts_alias(sels(i))
            sel_insts(i).match_vec  := insts_match(sels(i))
            sel_insts(i).inst_type  := insts_type(sels(i))
        }
    }
    need_2_cycles := PopCount(insts_valid) > decodeWidth.U
    for (i <- 0 until decodeWidth) {
        io.predec_resp(i).valid := RegEnable(i.U < numInsts, predec_req | fetch_nxt_block)
        io.predec_resp(i).bits := Mux(state_is_cycle_1, sel_insts(i), sel_insts(i + decodeWidth))
    }
    //  Update s1_pred_resp
    val predinfo_update = (io.s1_pred_resp.valid
      &  io.s1_pred_resp.bits.btb.inst_type
      & !io.s1_pred_resp.bits.btb.alias_err
      & (io.s1_pred_resp.bits.btb.addr(vaddrBits - 1, 4) === io.s1_bpu_pred.btb.addr)
      & (io.s1_pred_resp.bits.btb.addr(3, 0) >= io.s1_bpu_pred.btb.offset))
    val update_s1_bpu_pred = Wire(new s1_bpu_pred)
    update_s1_bpu_pred := io.s1_bpu_pred
    //
    val btb_taken = io.s1_pred_resp.bits.predictor.taken
    val base_w = io.s1_pred_resp.bits.predictor.table_base_w
    val wa_w = io.s1_pred_resp.bits.predictor.table_wa_w
    val wb_w = io.s1_pred_resp.bits.predictor.table_wb_w
    val wc_w = io.s1_pred_resp.bits.predictor.table_wc_w

    //  Update base table
    val base_w_inc = base_w < 3.S & btb_taken
    val base_w_dec = base_w > -3.S & !btb_taken
    update_s1_bpu_pred.predictor.table_base_w := Mux(base_w_inc, inc_ctr(base_w, base_w_inc, 1)
        ,   Mux(base_w_dec, inc_ctr(base_w, !base_w_dec, 1), base_w))

    //  Update wa table
    val wa_w_inc = wa_w < 13.S & btb_taken
    val wa_w_dec = wa_w > -13.S & !btb_taken
    update_s1_bpu_pred.predictor.table_wa_w := Mux(wa_w_inc, inc_ctr(wa_w, wa_w_inc, 3)
        ,   Mux(wa_w_dec, inc_ctr(wa_w, !wa_w_dec, 3), wa_w))


    //  Update wb table
    val wb_w_inc = wb_w < 7.S & btb_taken
    val wb_w_dec = wb_w > -7.S & !btb_taken
    update_s1_bpu_pred.predictor.table_wb_w := Mux(wb_w_inc, inc_ctr(wb_w, wb_w_inc, 1)
        ,   Mux(wb_w_dec, inc_ctr(wb_w, !wb_w_dec, 1), wb_w))


    //  Update wc table
    val wc_w_inc = wc_w < 15.S & btb_taken
    val wc_w_dec = wc_w > -15.S & !btb_taken
    update_s1_bpu_pred.predictor.table_wc_w := Mux(wc_w_inc, inc_ctr(wc_w, wc_w_inc, 1)
        ,   Mux(wc_w_dec, inc_ctr(wc_w, !wc_w_dec, 1), wc_w))

    //
    io.iq_bpu_pred := Mux(predinfo_update, RegNext(update_s1_bpu_pred), RegNext(io.s1_bpu_pred))
    io.exc_cause := RegNext(Fill(xLen, 0.U) & io.bypass_req.bits.cause)

    //  Counter
    val block_id = Reg(UInt(2.W))
    when (predec_req | fetch_nxt_block) {
        block_id := io.fetch_pc(5, 4)
    }
    io.block_done := fetch_nxt_block
    io.line_done := (block_id === 3.U(2.W)) & last_cycle
}