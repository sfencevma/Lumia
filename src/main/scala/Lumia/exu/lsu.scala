package Lumia.exu

import Chisel._
import freechips.rocketchip.rocket.{AMOALU, Causes, DecodeLogic}
import freechips.rocketchip.config.Parameters
import Lumia.cache._
import Lumia.dispatch._
import Lumia.common._
import Lumia.utils._
import freechips.rocketchip.util.MaskGen


class SniffReq(implicit p: Parameters) extends LumiaBundle {
    val rob_id   = UInt(width = robIdBits)
    val mem_size = UInt(width = MEM_SZ)
    val vaddr    = UInt(width = vaddrBits)
}

class SniffResp(implicit p: Parameters) extends LumiaBundle {
    val valid = Bool()
    val rob_id = UInt(width = robIdBits)
    val data = UInt(width = regBits)
}

class StoreQueueReq(implicit p: Parameters) extends LumiaBundle {
    val rob_id = UInt(width = robIdBits)
    val st_val = Bool()
    val stq_id  = UInt(width = stqIdBits)
    val uop     = UInt(width = FU_SZ)
    val mem_size = UInt(width = MEM_SZ)
}


class StoreQueueIO(
    dataBytes: Int,
    slot: Boolean = false
)(implicit p: Parameters) extends LumiaBundle {
    val kill = new Bundle() {
        val trap_kill = Bool(INPUT)
        val bjp_kill = Bool(INPUT)
        val bjp_rob_id = UInt(INPUT, width = robIdBits)
    }
    val enq = if (slot) Valid(new StoreQueueReq).flip else Vec(dispatchWidth, Valid(new StoreQueueReq).flip)
    //  Load sniff
    val sniff_req = Valid(new SniffReq).flip
    val sniff_resp = new SniffResp().asOutput
    //  Exec
    val exec = Valid(new Bundle() {
        val stq_id = if (slot) null else UInt(width = stqIdBits)
        val cr_val = Bool()
        val vaddr = UInt(width = vaddrBits)
        val cause = UInt(width = xLen)
    }).flip

    val exec_done = Valid(new Bundle() {
        val rob_id = UInt(width = robIdBits)
        val cause = UInt(width = xLen)
    })
    //  TLB request
    val tlb_resp = Decoupled(UInt(width = vaddrBits))
    val ppn = UInt(INPUT, width = ppnBits)
    //  Write to dcache
    val write = Decoupled(new Bundle() {
        val mask = UInt(width = dataBytes)
        val paddr = UInt(width = paddrBits)
        val data = UInt(width = regBits)
    })
    //
    val commit = if (slot) Bool(OUTPUT) else null
    val retire = Bool(INPUT)

    //
    val stq_dsp_ptr = if (slot) null else UInt(INPUT, width = stqIdBits)
    val stq_ret_ptr = if (slot) null else UInt(INPUT, width = stqIdBits)
    val stq_exc_ptr = if (slot) null else UInt(INPUT, width = stqIdBits)
    val stq_exc_done = if (slot) null else Bool(OUTPUT)
}

/*
class StoreQueueSlot(implicit p: Parameters) extends LumiaModule {
    private val dataBytes = p(LumiaTileKey).dcache.get.blockBits / 8
    val io = IO(new StoreQueueIO(dataBytes, true))

    def genMask(lo: UInt, ms: UInt, dw: Int, fp: Bool): (UInt, UInt) = {
        val mem_size = Mux(fp, ms, ms - 1.U)
        val off = log2Ceil(dw) - 1
        val mask = MaskGen(lo, mem_size, 2 * dw) << lo(off, 0)
        (mask(2 * dw - 1, dw), mask(dw - 1, 0))
    }*/
/*
    def genData(lo: UInt, d: UInt): (UInt, UInt) = {

    }
*//*
    //  Fields
    val valid = Reg(init = Bool(false))
    val done = Reg(init = Bool(false))
    val float = Reg(Bool())
    val cr_val = Reg(Bool())
    val cr_done = Reg(UInt(width = 2))
    val rob_id = Reg(UInt(width = robIdBits))
    val mem_size = Reg(UInt(width = vaddrBits))
    val vaddr = Reg(UInt(width = vaddrBits))
    val data = Reg(UInt(width = regBits))
    val pa_done = Reg(UInt(width = 2))
    val ppn_1 = Reg(UInt(width = ppnBits))
    val ppn_0 = Reg(UInt(width = ppnBits))
    val cause = Reg(UInt(width = xLen))

    val kill = io.kill.trap_kill || (io.kill.bjp_kill && valid && rob_old(io.kill.bjp_rob_id, rob_id))

    when (kill) {
        valid := false.B
    } .elsewhen (io.enq.valid) {
        //  Enq
        valid := true.B
        rob_id := io.enq.bits.rob_id
        mem_size := io.enq.bits.mem_size
        float :=
            (io.enq.bits.uop === uopFLW) ||
            (io.enq.bits.uop === uopFLD) ||
            (io.enq.bits.uop === uopFSW) ||
            (io.enq.bits.uop === uopFSD)

        //  Clear
        cr_done :=  0.U
        pa_done := 0.U
        cause := 0.U
        done := false.B
    }

    val has_cause = cause.orR()
    //  Sniff
    val older = rob_old(rob_id, io.sniff_req.bits.rob_id)
    val contains = io.sniff_req.bits.mem_size <= mem_size
    io.sniff_resp.valid := valid && done && older && contains && (vaddr === io.sniff_req.bits.vaddr)
    io.sniff_resp.data := data
    io.sniff_resp.rob_id := rob_id
    //  Exec
    when (io.exec.valid) {
        done := true.B
        cr_val := io.exec.bits.cr_val
        vaddr := io.exec.bits.vaddr
        cause := io.exec.bits.cause
    }
    //  DTLB request
    io.tlb_resp.valid := valid && done && ((!cr_val & !pa_done(0)) || (cr_val && !pa_done.andR())) && !has_cause
    io.tlb_resp.bits := Mux(pa_done(0), Cat(vaddr(vaddrBits - 1, 12) + 1.U, 0.U), vaddr)
    when (io.tlb_resp.ready) {
        pa_done := Cat(pa_done(0), io.tlb_resp.ready)
        ppn_0 := Mux(pa_done(0), ppn_0, io.ppn)
        ppn_1 := Mux(pa_done(1), ppn_1, io.ppn)
    }
    //  Write to DCache
    val (page_1_mask, page_0_mask) = genMask(vaddr(5, 0), mem_size, dataBytes, float)
    io.write.valid := valid && ((!cr_val & !cr_done(0) & pa_done(0)) || (cr_val & !cr_done.andR() & pa_done.orR())) & !has_cause
    io.write.bits.mask := Mux(cr_done(0), page_1_mask, page_0_mask)
    io.write.bits.data := data
    io.write.bits.paddr := Mux(cr_done(0), Cat(ppn_1, vaddr(11, 0)), Cat(ppn_0, vaddr(11, 0)))
    when (io.write.ready) {
        cr_done := Cat(cr_done, io.write.ready)
    }
    //  Retire
    when (io.retire) { valid := false.B }
    //  Store exec done
    io.exec_done.valid := (cr_val && cr_done(1)) || (!cr_val & cr_done) || has_cause
    io.exec_done.bits.rob_id := rob_id
    io.exec_done.bits.cause := cause
    //  Commit
    io.commit := valid & done & ((cr_val & pa_done.andR()) || (!cr_val & pa_done(1)))
}

class StoreQueue(dataBytes: Int)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new StoreQueueIO(dataBytes, false))

    def genIdx(id: UInt) = id(stqIdBits - 2, 0)
    def getYoungest(seq: Seq[SniffResp]): SniffResp = {
        val res = seq.fold(0.U.asTypeOf(new SniffResp))({ case (z, i) => {
            Mux(z.valid, Mux(i.valid, Mux(rob_old(z.rob_id, i.rob_id), i, z), i), i)
        }})
        res
    }

    //  Store queue
    val slots = for (s <- 0 until numStqEntries) yield { val slot = Module(new StoreQueueSlot); slot }
    val stq = Vec(slots.map(_.io))
    for (slot <- stq) {
        slot.kill := io.kill
        slot.sniff_req := io.sniff_req
    }

    //  Enq
    for (s <- 0 until numStqEntries) {
        val pos_oh = io.enq.map(i => i.valid && i.bits.st_val && s.U === genIdx(i.bits.stq_id))
        stq(s).enq := Mux1H(pos_oh, io.enq)
    }

    //  Load sniff
    io.sniff_resp.valid := !stq.map(_.sniff_req.valid).reduce(_|_)
    io.sniff_resp.rob_id := chisel3.DontCare
    io.sniff_resp.data := getYoungest(stq.map(_.sniff_resp)).data

    //  Exec logic
    val exec_id = genIdx(io.exec.bits.stq_id)
    stq(exec_id).exec.valid := io.exec.valid
    stq(exec_id).exec.bits.cr_val := io.exec.bits.cr_val
    stq(exec_id).exec.bits.vaddr := io.exec.bits.vaddr
    stq(exec_id).exec.bits.cause := io.exec.bits.cause

    //  TLB request
    val sel_exec = genIdx(io.stq_exc_ptr)
    io.tlb_resp := stq(sel_exec).tlb_resp
    stq(sel_exec).ppn := io.ppn

    //  Write dcache
    io.write := stq(sel_exec).write

    io.exec_done := stq(sel_exec).exec_done
    io.stq_exc_done := io.commit

    //  Retire
    stq.zipWithIndex.map { case (v, i) => v.retire := genIdx(io.stq_ret_ptr) === i.U && io.retire }
}

class AGU(implicit p: Parameters) extends LumiaModule {
    val io = IO(new Bundle() {
        val st_val = Bool(INPUT)
        val ld_val = Bool(INPUT)
        val amo    = Bool(INPUT)
        val rs1    = UInt(INPUT, width = regBits)
        val ip     = UInt(INPUT, width = LONG_IMM_SZ)
        val mem_size = UInt(INPUT, width = MEM_SZ)
        val cr_val = Bool(OUTPUT)
        val vaddr = UInt(OUTPUT, width = vaddrBits + 1)
        val cause = UInt(OUTPUT, width = xLen)
    })

    def cross_page(vaddr: UInt, mem_size: UInt) = {

    }
    def checkExceptions(x: Seq[(Bool, UInt)]) = (x.map(_._1).reduce(_||_), PriorityMux(x))

    val vaddr = (io.rs1.asSInt() + io.ip(19, 8).asSInt()).asUInt()
    val ea_sign = Mux(vaddr(vaddrBits - 1), (~vaddr(63, vaddrBits)).asUInt() === 0.U, vaddr(63, vaddrBits) =/= 0.U)
    val eff_vaddr = Cat(ea_sign, vaddr(vaddrBits - 1, 0)).asUInt()
    val size = io.mem_size
    val misaligned =
          (size === MEM_B && (eff_vaddr(0)    =/= 0.U)) ||
          (size === MEM_H && (eff_vaddr(1, 0) =/= 0.U)) ||
          (size === MEM_W && (eff_vaddr(2, 0) =/= 0.U))

    val ma_ld = io.ld_val && misaligned
    val ma_st = (io.st_val || io.amo) && misaligned
    val (cause_val, cause) = checkExceptions(List(
        (ma_ld, (Causes.misaligned_load).U),
        (ma_st, (Causes.misaligned_store).U)
    ))
    io.cause := Fill(xLen, cause_val) & cause
    io.cr_val := cross_page(io.vaddr, io.mem_size)
}

trait HasLsuCtrlSigs {
    val float   = Bool()
    val amo     = Bool()
    val ld      = Bool()
    val st      = Bool()
}

trait HasAmoCtrlSigs {
    val lr      = Bool()
    val sc      = Bool()
    val swap    = Bool()
    val add     = Bool()
    val or      = Bool()
    val min     = Bool()
    val max     = Bool()
}

class LsuCtrlSigs extends Bundle with HasLsuCtrlSigs
class AmoCtrlSigs extends Bundle with HasAmoCtrlSigs

class LsuDecoder(implicit p: Parameters) extends LumiaModule {
    val io = IO(new Bundle() {
        val uop = UInt(INPUT, width = uopBits)
        val sigs = new LsuCtrlSigs().asOutput
    })
    val default = List[BitPat](X, X, X, X)
    val insns = Array[(BitPat, List[BitPat])] (
        BitPat(uopFLW)  -> List(Y, N, Y, N),
        BitPat(uopFSW)  -> List(Y, N, N, Y),
        BitPat(uopFLD)  -> List(Y, N, Y, N),
        BitPat(uopFSD)  -> List(Y, N, N, Y),
        BitPat(uopLOAD) -> List(N, N, Y, N),
        BitPat(uopSTORE)-> List(N, N, N, Y),
        BitPat(uopLRW)  -> List(N, Y, Y, N),
        BitPat(uopSCW)  -> List(N, Y, N, N),
        BitPat(uopAMOSWAP_W)    -> List(N, Y, N, N),
        BitPat(uopAMOADD_W)     -> List(N, Y, N, N),
        BitPat(uopAMOXOR_W)     -> List(N, Y, N, N),
        BitPat(uopAMOAND_W)     -> List(N, Y, N, N),
        BitPat(uopAMOOR_W)      -> List(N, Y, N, N),
        BitPat(uopAMOMIN_W)     -> List(N, Y, N, N),
        BitPat(uopAMOMAX_W)     -> List(N, Y, N, N)
    )
    val decoder = DecodeLogic(io.uop, default, insns)
    val s = io.sigs
    val sigs = Seq(s.float, s.amo, s.ld, s.st)
    sigs zip decoder map { case (f, s) => f := s }
}

class AmoDecoder(implicit p: Parameters) extends LumiaModule {
    val io = IO(new Bundle() {
        val uop = UInt(INPUT, width = uopBits)
        val sigs = new AmoCtrlSigs().asOutput
    })

    val default = List[BitPat](X, X, X, X, X, X, X)
    val insns = Array[(BitPat, List[BitPat])] (
        BitPat(uopLRW)          -> List(Y, N, N, N, N, N, N),
        BitPat(uopSCW)          -> List(N, Y, N, N, N, N, N),
        BitPat(uopAMOSWAP_W)    -> List(N, N, Y, N, N, N, N),
        BitPat(uopAMOADD_W)     -> List(N, N, N, Y, N, N, N),
        BitPat(uopAMOOR_W)      -> List(N, N, N, N, Y, N, N),
        BitPat(uopAMOMIN_W)     -> List(N, N, N, N, N, Y, N),
        BitPat(uopAMOMAX_W)     -> List(N, N, N, N, N, N, Y)
    )

    val decoder = DecodeLogic(io.uop, default, insns)
    val s = io.sigs
    val sigs = Seq(s.lr, s.sc, s.swap, s.add, s.or, s.min, s.max)
    sigs zip decoder map { case (f, s) => f := s }
}

class LSU(implicit p: Parameters) extends ExecUnit (
    readIrf = true,
    readFrf = true,
    writeIrf = true,
    writeFrf = true,
    hasMem = true,
    dataWidth = 65
) {
    private val params = p(LumiaTileKey).dcache.get
    //  Decoding
    val lsu_decoder = Module(new LsuDecoder)
    val amo_decoder = Module(new AmoDecoder)

    lsu_decoder.io.uop := io.req.bits.uop.uop
    amo_decoder.io.uop := io.req.bits.uop.uop

    val lsu_ctrl = lsu_decoder.io.sigs
    val amo_ctrl = amo_decoder.io.sigs

    //  Calculate address
    val agu = Module(new AGU)
    agu.io.st_val := lsu_ctrl.st
    agu.io.ld_val := lsu_ctrl.ld
    agu.io.amo := lsu_ctrl.amo
    agu.io.rs1 := io.req.bits.rs1_data
    agu.io.ip := io.req.bits.uop.imm_package
    agu.io.mem_size := io.req.bits.uop.mem_size

    val stq = Module(new StoreQueue(regBits)).io
    stq.enq := io.lsu_io.enq
    stq.exec.valid := io.req.valid && lsu_ctrl.st
    stq.exec.bits.stq_id := io.req.bits.ids.stq_id
    stq.exec.bits.cr_val := agu.io.cr_val
    stq.exec.bits.vaddr := agu.io.vaddr
    stq.exec.bits.cause := agu.io.cause


    //  Trans

    val dtlb = Module(new TLB(params.nTLBWays, params.nTLBSets)).io
    val dcache = Module(new DCache()).io

    //  AMOALU
    val amoalu = Module(new AMOALU(xLen))
}*/