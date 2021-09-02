package Lumia.iq

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.predecoder._
import Lumia.bpu._
import Lumia.common._
import Lumia.utils._

class s2_bpu_pred(implicit p: Parameters) extends LumiaBundle
  with HasBtacParameters
  with HasPhtParameters
{
    val btac        = new BtacResp
    val pht         = new PhtResp
    val taken       = Bool()
}

class s2_pred_resp(implicit p: Parameters) extends LumiaBundle
  with HasBtacParameters
  with HasPhtParameters
{
    val btac        = new BtacUpdate
    val pht         = new PhtUpdateReq
    val taken       = Bool()
}

class IqResp(implicit p: Parameters) extends LumiaBundle
  with HasBtacParameters
  with HasPhtParameters
{
    val inst            = UInt(width = instBits)
    val len        = Bool()
    val addr       = UInt(width = vaddrBits)
    val inst_taddr      = UInt(width = vaddrBits)
    val cause           = UInt(width = xLen)
    val s2_bpu_pred     = new s2_bpu_pred
}

class QueueEntry(implicit p: Parameters) extends LumiaBundle
  with HasBtacParameters
  with HasPhtParameters
{
    val inst        = UInt(width = instBits)
    val addr        = UInt(width = vaddrBits)
    val len         = Bool()
    val cause   = UInt(width = xLen.W)
    val s2_bpu_pred = new s2_bpu_pred
    val taddr       = UInt(width = vaddrBits)
    val inst_type   = Bool()
}

class InstQueueIO(numPorts: Int)(implicit p: Parameters) extends LumiaBundle
  with HasBtacParameters
  with HasPhtParameters
{
    val kill          = new Bundle() {
        val trap_kill   = Bool(INPUT)
        val bjp_kill    = Bool(INPUT)
    }
    val stall         = Bool(INPUT)
    val prv           = UInt(INPUT, width = 2)
    val insts         = Vec(numPorts, Valid(new PreDecoderResp))
    val s1_bpu_pred   = new s1_bpu_pred().asInput
    val cause         = UInt(INPUT, width = xLen)
    val s2_pred_resp  = Valid(new s2_pred_resp).flip

    val resp          = Vec(numPorts, Valid(new IqResp))
    val s1_pred_resp  = Valid(new s1_pred_resp)
    val s2_bpu_kill   = Valid(UInt(width = vaddrBits))
    val iq_uc_kill    = Valid(UInt(width = vaddrBits))
    
    val iq_stall      = Bool(OUTPUT)
}



class InstQueue(implicit p: Parameters) extends LumiaModule
  with HasBtacParameters
  with HasPhtParameters
{
    val io = IO(new InstQueueIO(decodeWidth))

    def getImm(inst: UInt) = {
        val jal     = inst(6, 0) === "b1101111".U
        val cjal    = inst(1, 0) === "b01".U & inst(15, 13) === "b001".U
        val cj      = inst(1, 0) === "b01".U & inst(15, 13) === "b101".U
        val jal_imm = Cat(Fill(11, inst(31)), inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W))
        val cj_imm  = Cat(Fill(20, inst(12)), inst(12), inst(8), inst(10, 9), inst(6), inst(7), inst(2), inst(11), inst(5, 3), 0.U(1.W))
        Mux(jal, jal_imm, Mux(cjal | cj, cj_imm, 0.U))
    }
    def isBranch(inst: UInt) = {
        val _32Branch = inst(6, 0) === "b1100011".U
        val _16Branch = inst(1, 0) === "b01".U & inst(15, 14) === "b11".U
        _32Branch | _16Branch
    }
    def isJump(inst: UInt) = {
        val jal     = inst(6, 0) === "b1101111".U
        val cjal    = inst(1, 0) === "b01".U & inst(15, 13) === "b001".U
        val cj      = inst(1, 0) === "b01".U & inst(15, 13) === "b101".U
        jal | cjal | cj
    }
    def byteAlign(alias_vec: UInt, match_vec: UInt, offset: UInt, len: UInt) = {
        val already_align = match_vec.orR()
        offset - Mux(already_align, Mux(len === 1.U, 3.U, 1.U), OHToUInt(alias_vec))
    }
    private val queueEntrySz = (new QueueEntry).getWidth
    val kill = io.kill.trap_kill || io.kill.bjp_kill

    def genIdx(id: UInt) = id(log2Ceil(numIqEntries) - 2, 0)
    //*********************************************
    //  Module body
    val btac = Module(new Btac(decodeWidth)).io
    btac.prv := io.prv
    for (p <- 0 until decodeWidth) {
        btac.req(p).valid := io.insts(p).valid
        btac.req(p).bits.addr := io.insts(p).bits.addr
        btac.req(p).bits.len := io.insts(p).bits.len
    }

    btac.update.valid := io.s2_pred_resp.valid
    btac.update.bits := io.s2_pred_resp.bits.btac


    //  PHT
    val pht = Module(new Pht(decodeWidth)).io
    for (p <- 0 until dispatchWidth) {
        pht.req(p).valid := io.insts(p).valid
        pht.req(p).bits := io.insts(p).bits.addr
    }
    pht.update.valid := io.s2_pred_resp.valid & io.s2_pred_resp.bits.btac.inst_type
    pht.update.bits := io.s2_pred_resp.bits.pht

    //  Instruction queue
    val instq = chisel3.SyncReadMem(numIqEntries, UInt(queueEntrySz.W))
    val wr_ptr = RegInit(0.U((log2Ceil(numIqEntries) + 1).W))
    val rd_ptr = RegInit(0.U((log2Ceil(numIqEntries) + 1).W))


    val new_entries = Wire(Vec(decodeWidth, new QueueEntry))
    for (p <- 0 until decodeWidth) {
        new_entries(p).inst                       := io.insts(p).bits.inst
        new_entries(p).len                        := io.insts(p).bits.len
        new_entries(p).addr                       := io.insts(p).bits.addr
        new_entries(p).cause                  := io.cause & Fill(decodeWidth, 0.U === p.U)
        new_entries(p).s2_bpu_pred.btac.hit       := btac.resp(p).hit
        new_entries(p).s2_bpu_pred.btac.index     := btac.resp(p).index
        new_entries(p).s2_bpu_pred.btac.inst_type := btac.resp(p).inst_type
        new_entries(p).s2_bpu_pred.pht.index      := pht.resp(p).index
        new_entries(p).s2_bpu_pred.pht.status     := pht.resp(p).status
        new_entries(p).s2_bpu_pred.taken          := !btac.resp(p).inst_type || pht.resp(p).status(1)
        new_entries(p).taddr                      := btac.resp(p).taddr
        new_entries(p).inst_type                  := io.insts(p).bits.inst_type(1, 0).orR()
    }

    //  Alias check
    val alias_err = Wire(Vec(decodeWidth, Bool()))
    for (p <- 0 until decodeWidth) {
        val bits = io.insts(p).bits
        alias_err(p) := ((!bits.len & bits.alias_vec(0) | (bits.len & bits.alias_vec(2, 0).orR())
          |   (!bits.len & bits.match_vec(1) & (!bits.inst_type(5, 2).orR())))
          |   (!bits.len & bits.match_vec(1) & ((bits.inst_type(3, 2).orR() & io.s1_bpu_pred.btb.inst_type) | (bits.inst_type(5, 4).orR() & !io.s1_bpu_pred.btb.inst_type)))
          |   (bits.len & bits.match_vec(3) & !bits.inst_type(5, 2))
          |   (bits.len & bits.match_vec(3) & (bits.inst_type(3, 2).orR() & io.s1_bpu_pred.btb.inst_type) | (bits.inst_type(5, 4).orR() & !io.s1_bpu_pred.btb.inst_type)))
    }

    val alias_err_check = io.insts zip alias_err map { case (f, s) => f.valid & s } reduce (_|_)

    //  Generate write index
    val instq_wr_idx = Wire(Vec(decodeWidth, UInt(log2Ceil(numIqEntries).W)))
    val instq_req = Cat(io.insts.map(_.valid).reverse)

    instq_wr_idx(0) := wr_ptr
    for (p <- 1 until decodeWidth) {
        instq_wr_idx(p) := wr_ptr + PopCount(instq_req(p - 1, 0))
    }

    //  Write
    val btack_taken = new_entries.map(_.asTypeOf(new QueueEntry).s2_bpu_pred.taken)
    val wr_valid = Wire(Vec(decodeWidth, Bool()))

    wr_valid(0) := io.insts(0).valid & (!alias_err(0) | io.cause.orR())
    wr_valid(1) := io.insts(1).valid & !alias_err.asUInt()(1, 0).orR() & !btack_taken(0)
    wr_valid(2) := io.insts(2).valid & !alias_err.asUInt()(2, 0).orR() & !(btack_taken(0) | btack_taken(1)) & !io.insts(0).bits.inst_type(3, 2).orR()
    wr_valid(3) := io.insts(3).valid & !alias_err.asUInt()(3, 0).orR() & !(btack_taken(0) | btack_taken(1) | btack_taken(2))

    when (kill) {
        wr_ptr := 0.U
    } .elsewhen (io.stall) {
        wr_ptr := wr_ptr + PopCount(wr_valid)
    }

    for (p <- 0 until decodeWidth) {
        when (io.stall & !kill & wr_valid(p)) {
            instq.write(genIdx(instq_wr_idx(p)), new_entries(p).asUInt())
        }
    }

    //  Select logic
    val numInsts = wr_ptr - rd_ptr
    val s1_valid = Wire(Vec(decodeWidth, Bool()))
    val instq_rd_idx = (0 until decodeWidth) map { case i =>
        rd_ptr + i.U
    }

    when (kill) {
        rd_ptr := 0.U
    } .elsewhen (!io.stall) {
        rd_ptr := rd_ptr + PopCount(s1_valid)
    }

    val select_insts = instq_rd_idx.map(s => instq(genIdx(s)).asTypeOf(new QueueEntry))
    s1_valid(0) := numInsts > 0.U
    s1_valid(1) := numInsts > 1.U & !select_insts(0).inst_type
    s1_valid(2) := numInsts > 2.U & !(select_insts(0).inst_type | select_insts(1).inst_type)
    s1_valid(3) := numInsts > 3.U & !(select_insts(0).inst_type | select_insts(1).inst_type | select_insts(2).inst_type)

    //  Bypass
    val s2_valid = Wire(Vec(decodeWidth, Bool()))
    s2_valid(0) := numInsts === 0.U & wr_valid(0)
    s2_valid(1) := numInsts === 0.U & wr_valid(1) & !io.insts(0).bits.inst_type(1, 0).orR()
    s2_valid(2) := numInsts === 0.U & wr_valid(2) & !io.insts(0).bits.inst_type(1, 0).orR() & !io.insts(1).bits.inst_type(1, 0).orR()
    s2_valid(3) := numInsts === 0.U & wr_valid(3) & !io.insts(0).bits.inst_type(1, 0).orR() & !io.insts(1).bits.inst_type(1, 0).orR() & !io.insts(2).bits.inst_type(1, 0).orR()

    //
    val iq_req_valid = Wire(Vec(decodeWidth, Bool()))
    for (p <- 0 until decodeWidth) {
        when (kill) {
            iq_req_valid(p) := false.B
        } .elsewhen (!io.stall & (s1_valid(p) | s2_valid(p))) {
            iq_req_valid(p) := true.B
        } .otherwise {
            iq_req_valid(p) := false.B
        }
    }

    val iq_req_bits = s1_valid.zipWithIndex map { case (v, i) =>
        Mux(v, instq.read(genIdx(instq_rd_idx(i)), v).asTypeOf(new QueueEntry), RegNext(new_entries(i).asTypeOf(new QueueEntry)))
    }

    io.resp zip iq_req_valid map { case (f, s) =>
        f.valid := RegNext(s)
    }
    io.resp zip iq_req_bits map { case (f, s) =>
        f.bits.inst := s.inst
        f.bits.len := s.len
        f.bits.addr := s.addr
        f.bits.inst_taddr := s.taddr
        f.bits.cause := s.cause
        f.bits.s2_bpu_pred := s.s2_bpu_pred
    }

    //  Update bpu
    //  Uncondition flush
    val uc_valid = io.insts.map(s => isJump(s.bits.inst)) zip wr_valid map { case (f, s) => f & s }
    val uc_imm = io.insts.map(s => getImm(s.bits.inst))
    val has_uc = uc_valid.reduce(_|_)
    val sel_uc = select_first(Cat(uc_valid.reverse), 1).map(OHToUInt(_))
    val uc_pc_addr = io.insts(sel_uc(0)).bits.addr + Mux1H(UIntToOH(sel_uc(0)), uc_imm)
    val sel_alias = select_first(Cat(alias_err.reverse), 1).map(OHToUInt(_))
    val alias_pc_addr = (Cat(io.insts(0).bits.addr(vaddrBits - 1, 4), 0.U(4.W))
      + byteAlign(io.insts(sel_alias(0)).bits.alias_vec
        , io.insts(sel_alias(0)).bits.match_vec
        , io.s1_bpu_pred.btb.offset
        , io.insts(sel_alias(0)).bits.len))
    io.iq_uc_kill.valid := has_uc | alias_err_check
    io.iq_uc_kill.bits := Mux(alias_err_check, alias_pc_addr, uc_pc_addr)

    //  Condition flush
    val condiInst = btac.resp zip pht.resp map {
        case (b, p) => {
            b.hit & (!b.inst_type | p.status(1))
        }
    }
    val has_condi = io.insts zip condiInst map { case (f, s) => f.valid & s } reduce (_|_)
    val sel_condi = select_first(Cat(condiInst.reverse), 1).map(OHToUInt(_))
    io.s2_bpu_kill.valid := has_condi
    io.s2_bpu_kill.bits := btac.resp(sel_condi(0)).taddr

    //  Update BPU
    io.s1_pred_resp.valid := io.insts.map(_.valid).reduce(_|_)
    
    val btb_update  = new BTBUpdate
    btb_update.br_new    := !io.s1_bpu_pred.btb.hit
    btb_update.addr      := Cat(alias_pc_addr(vaddrBits - 1, 4)
                            ,   alias_pc_addr(4, 0) + Mux(io.insts(sel_alias(0)).bits.len, 3.U, 1.U))
    btb_update.taddr     := uc_pc_addr
    btb_update.inst_type := !has_uc
    btb_update.index     := io.s1_bpu_pred.btb.index
    btb_update.alias_err := alias_err_check
    btb_update.tsucc     := !btac.resp(sel_condi(0)).hit | (btac.resp(sel_condi(0)).taddr === io.s1_bpu_pred.btb.taddr)
    
    val predictor_update = new PredictorUpdate
    predictor_update := io.s1_bpu_pred.predictor
    predictor_update.taken := has_uc

    io.s1_pred_resp.bits.btb := btb_update
    io.s1_pred_resp.bits.predictor := predictor_update
    
    //
    io.iq_stall := io.stall
}


