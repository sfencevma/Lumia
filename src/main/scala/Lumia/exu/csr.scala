package Lumia.exu

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.rocket.{PMP, PMPConfig, CSR, CSRs, Causes}
import Lumia.common._
import scala.collection.mutable.LinkedHashMap

import freechips.rocketchip.config.Parameters

object CSR {
    val ADDRSZ = 12
    val busErrorIntCause = 128
    val debugIntCause = 14
    val debugTriggerCause = debugIntCause
}

class MStatus extends Bundle {
    val dprv = UInt(width = PRV.PRV_SZ)
    val prv = UInt(width = PRV.PRV_SZ)
    val sd = Bool()
    val zero = UInt(width = 8)
    val tsr = Bool()
    val tw = Bool()
    val tvm = Bool()
    val mxr = Bool()
    val sum = Bool()
    val mprv = Bool()
    val xs = UInt(width = 2)
    val fs = UInt(width = 2)
    val mpp = UInt(width = 2)
    val vs = UInt(width = 2)
    val spp = UInt(width = 1)
    val mpie = Bool()
    val hpie = Bool()
    val spie = Bool()
    val upie = Bool()
    val mie = Bool()
    val hie = Bool()
    val sie = Bool()
    val uie = Bool()
}

class DCSR extends Bundle {
    val xdebugver = UInt(width = 2)
    val zero4 = UInt(width = 2)
    val zero3 = UInt(width = 12)
    val ebreakm = Bool()
    val ebreakh = Bool()
    val ebreaks = Bool()
    val ebreaku = Bool()
    val zero2 = Bool()
    val stopcycle = Bool()
    val stoptime = Bool()
    val cause = UInt(width = 3)
    val zero1 = UInt(width = 3)
    val step = Bool()
    val prv = UInt(width = PRV.PRV_SZ)
}

class MIP extends Bundle {
    val zero = UInt(width = 20)
    val meip = Bool()
    val zero1 = Bool()
    val seip = Bool()
    val zero2 = Bool()
    val mtip = Bool()
    val zero3 = Bool()
    val stip = Bool()
    val zero4 = Bool()
    val msip = Bool()
    val zero5 = Bool()
    val ssip = Bool()
    val zero6 = Bool()
}

class Satp(implicit p: Parameters) extends LumiaBundle {
    val mode = Bool()
    val asid = UInt(asidBits.W)
    val ppn = UInt(ppnBits.W)
}

class PMPReg(implicit p: Parameters) extends LumiaBundle {
    val cfg = new PMPConfig
    val addr = UInt(width = paddrBits - PMP.lgAlign)
}

class Interrupts extends Bundle {
    val debug = Bool()
    val mtip = Bool()
    val msip = Bool()
    val meip = Bool()
    val seip = Bool()
    val buserror = UInt(CSR.busErrorIntCause.W)
}

case class WideCounter(width: Int, inc: UInt = UInt(1), reset: Boolean = true) {
    private val isWide = width > 2*inc.getWidth
    private val smallWidth = if (isWide) inc.getWidth max log2Up(width) else width
    private val small = if (reset) Reg(init=UInt(0, smallWidth)) else Reg(UInt(width = smallWidth))
    private val nextSmall = small +& inc
    private val ctrWidth = width
    small := nextSmall

    private val large = if (isWide) {
        val r = if (reset) Reg(init=UInt(0, width - smallWidth)) else Reg(UInt(width = width - smallWidth))
        when (nextSmall(smallWidth)) { r := r + UInt(1) }
        r
    } else null

    val value = if (isWide) Cat(large, small) else small
    lazy val carryOut = {
        val lo = (small ^ nextSmall) >> 1
        if (!isWide) lo else {
            val hi = Mux(nextSmall(smallWidth), large ^ (large +& UInt(1)), UInt(0)) >> 1
            Cat(hi, lo)
        }
    }

    def := (x: UInt) = {
        small := x
        if (isWide) large := x >> smallWidth
    }

    def getWidth = ctrWidth
}

class CSRIn(implicit p: Parameters) extends LumiaBundle {
    //  System instruction
    val ecall = Bool(INPUT)
    val ebreak = Bool(INPUT)
    val mret = Bool(INPUT)
    val sret = Bool(INPUT)
    val uret = Bool(INPUT)
    val dret = Bool(INPUT)
    val wfi = Bool(INPUT)
    val sfence = Bool(INPUT)

    //  Interrupts & exceptions
    val interrupts = new Interrupts().asInput
    val exception = Bool(INPUT)
    val exc_len = Bool(INPUT)
    val exc_cause = UInt(INPUT, width = xLen)
    val exc_pc = UInt(INPUT, width = vaddrBits)
    val badvaddr = UInt(INPUT, width = vaddrBits)

    //  Retire
    val retire = UInt(INPUT, width = log2Ceil(1 + retireWidth))
    //  FPU
    val set_fs_dirty = Bool(INPUT)
    val fcsr_fflags = Valid(UInt(width = 5)).flip
}

class CSROut(implicit p: Parameters) extends LumiaBundle {
    val fcsr_frm = UInt(OUTPUT, width = 3)
    val singleStep = Bool(OUTPUT)
    //  Trap
    val trap_kill = Bool(OUTPUT)
    val trap_addr = UInt(OUTPUT, width = vaddrBits)
    //
    val prv = UInt(OUTPUT, width = PRV.PRV_SZ)
    val satp = new Satp().asOutput
    val csr_stall = Bool(OUTPUT)
}

class CSR(implicit p: Parameters) extends LumiaModule {
    val io = IO(new Bundle () {
        val in = new CSRIn
        val out = new CSROut
        //  Read & Write
        val rw = new Bundle {
            val wren = Bool(INPUT)
            val addr = UInt(INPUT, width = CSR.ADDRSZ)
            val rdata = UInt(OUTPUT, width = xLen)
            val wdata = UInt(INPUT, width = xLen)
        }
    })
    //
    def isaStringToMask(s: String) = s.map(c => 1 << (c - 'A')).foldLeft(0)(_|_)
    def readEPC(x: UInt) = ~((~x).asUInt() | 1.U)
    def formEPC(x: UInt) = ~((~x).asUInt() | 1.U)
    def pgLevelsToMode(i: Int) = (xLen, i) match {
        case (32, 2) => 1
        case (64, x) if x >= 3 && x <= 6  => x + 5
    }
    def chooseInterrupt(sup: UInt, masksIn: Seq[Bits]): (Bool, Bits) = {
        val nonstandard = sup.getWidth - 1 to 12 by -1
        val standard = Seq(11, 3, 7, 9, 1, 5, 8, 0, 4)
        val priority = nonstandard ++ standard
        val masks = masksIn.reverse
        val any = masks.flatMap(m => priority.filter(_ < m.getWidth).map(i => m(i))).reduce(_|_)
        val which = PriorityMux(masks.flatMap(m => priority.filter(_ < m.getWidth).map(i => (m(i), i.U))))
        (any, which)
    }
    //  Machine level
    val isaMaskString = "M" + "C"
    val isaString = "I" + isaMaskString + "S" + "U"
    val isaMax = (BigInt(log2Ceil(xLen) - 4) << (xLen - 2)) | isaStringToMask(isaString)
    val reg_misa = Reg(isaMax.U(xLen.W))
    val reset_mstatus = Wire(init=new MStatus().fromBits(0))
    reset_mstatus.mpp := PRV.M
    reset_mstatus.prv := PRV.M
    val (reg_mstatus, read_mstatus) = {
        val reg = Reg(init = reset_mstatus)
        (reg, reg.asUInt()(xLen - 1, 0))
    }
    io.out.prv := reg_mstatus.prv
    val new_prv = Wire(init = reg_mstatus.prv)
    val (reg_mtvec, read_mtvec) = {
        val reg = Reg(UInt(xLen.W))
        (reg, reg.asUInt()(xLen - 1, 0))
    }
    val (supported_interrupts, delegable_interrupts) = {
        val sup = Wire(new MIP)
        sup.zero := 0.U
        sup.meip := true.B
        sup.zero1 := false.B
        sup.seip := true.B
        sup.zero2 := false.B
        sup.mtip := true.B
        sup.zero3 := false.B
        sup.stip := true.B
        sup.zero4 := false.B
        sup.msip := true.B
        sup.zero5 := false.B
        sup.ssip := true.B
        sup.zero6 := false.B
        val supported_high_interrupts = if (io.in.interrupts.buserror.asBools().nonEmpty) UInt(BigInt(1) << CSR.busErrorIntCause) else 0.U

        val del = Wire(init = sup)
        del.msip := false.B
        del.mtip := false.B
        del.meip := false.B
        (sup.asUInt() | supported_high_interrupts, del.asUInt())
    }
    val reg_mip = Reg(init = new MIP().fromBits(0))
    val mip = Wire(init = reg_mip)
    mip.mtip := io.in.interrupts.mtip
    mip.msip := io.in.interrupts.msip
    mip.meip := io.in.interrupts.meip
    mip.seip := reg_mip.seip || io.in.interrupts.seip
    val read_mip = mip.asUInt() & supported_interrupts
    val delegable_exceptions = UInt(Seq(
        Causes.misaligned_fetch
        ,   Causes.fetch_page_fault
        ,   Causes.breakpoint
        ,   Causes.load_page_fault
        ,   Causes.store_page_fault
        ,   Causes.misaligned_load
        ,   Causes.misaligned_store
        ,   Causes.illegal_instruction
        ,   Causes.user_ecall).map(1 << _).sum)
    val reg_mie = Reg(UInt(width = xLen))
    val (reg_medeleg, read_medeleg) = {
        val reg = Reg(UInt(xLen.W))
        (reg, reg & delegable_exceptions)
    }
    val (reg_mideleg, read_mideleg) = {
        val reg = Reg(UInt(xLen.W))
        (reg, reg & delegable_interrupts)
    }
    val delegable_counters = ((BigInt(1) << (coreParams.nPerfCounters + 3)) - 1).U
    val (reg_mcounteren, read_mcounteren) = {
        val reg = Reg(UInt(32.W))
        (reg, reg & delegable_counters)
    }

    val high_interrupts = io.in.interrupts.buserror.asBools().map(_ << CSR.busErrorIntCause)
    val pending_interrupts = Vec(high_interrupts).asUInt() | (read_mip & reg_mie)
    val d_interrupts = io.in.interrupts.debug << CSR.debugIntCause
    val m_interrupts = Mux(reg_mstatus.mprv <= PRV.S || reg_mstatus.mie, ~((~pending_interrupts).asUInt() | read_mideleg), UInt(0))
    val s_interrupts = Mux(reg_mstatus.mprv < PRV.S || (reg_mstatus.mprv === PRV.S && reg_mstatus.mie), pending_interrupts & read_mideleg, UInt(0))
    val (anyInterrupt, whichInterrupt) = chooseInterrupt(supported_interrupts, Seq(s_interrupts, m_interrupts, d_interrupts))
    val interruptMSB = BigInt(1) << (xLen - 1)
    val interruptCause = UInt(interruptMSB) + whichInterrupt.asUInt()
    val reg_mscratch = Reg(UInt(width = xLen))
    val reg_mepc = Reg(UInt(width = vaddrBits))
    val reg_mcause = Reg(Bits(width = xLen))
    val reg_mtval = Reg(UInt(width = vaddrBits))
    val reg_instret =  WideCounter(64, io.in.retire)
    val reg_cycle =   WideCounter(64, !io.out.csr_stall)
    val machine_read_mapping = LinkedHashMap[Int, Bits] (
        CSRs.misa -> reg_misa,
        CSRs.mstatus -> read_mstatus,
        CSRs.mtvec -> read_mtvec,
        CSRs.mip -> read_mip,
        CSRs.mie -> reg_mie,
        CSRs.mideleg -> read_mideleg,
        CSRs.medeleg -> read_medeleg,
        CSRs.mcounteren -> read_mcounteren,
        CSRs.mscratch -> reg_mscratch,
        CSRs.mepc -> readEPC(reg_mepc),
        CSRs.mcause -> reg_mcause,
        CSRs.mtval -> reg_mtval,
        CSRs.instret -> reg_instret.value.asUInt(),
        CSRs.cycle -> reg_cycle.value.asUInt(),
        CSRs.mcycle -> reg_cycle.value.asUInt(),
        CSRs.mcycleh -> (reg_cycle.value.asUInt() >> 32),
        CSRs.minstret -> (reg_cycle.value.asUInt() >> 32),
        CSRs.minstreth -> (reg_instret.value.asUInt() >> 32),
        CSRs.cycleh -> (reg_cycle.value.asUInt() >> 32),
        CSRs.instreth -> (reg_instret.value.asUInt() >> 32)
    )
    Seq(CSRs.mhartid, CSRs.mimpid, CSRs.marchid, CSRs.mvendorid).foreach(id => machine_read_mapping.getOrElseUpdate(id, 0.U))
    //  Supervisor level
    val read_sie = reg_mie & read_mideleg
    val read_sip = reg_mip.asUInt() & read_mideleg
    val sstatus = Wire(init = 0.U.asTypeOf(new MStatus))
    sstatus.sd := reg_mstatus.sd
    sstatus.mxr := reg_mstatus.mxr
    sstatus.sum := reg_mstatus.sum
    sstatus.xs := reg_mstatus.xs
    sstatus.fs := reg_mstatus.fs
    sstatus.vs := reg_mstatus.vs
    sstatus.spp := reg_mstatus.spp
    sstatus.spie := reg_mstatus.spie
    sstatus.sie := reg_mstatus.sie
    val read_sstatus = sstatus.asUInt()(xLen - 1, 0)
    val (reg_stvec, read_stvec) = {
        val reg = Reg(UInt(width = vaddrBits))
        (reg, reg.asUInt()(xLen - 1, 0))
    }
    val (reg_scounteren, read_scounteren) = {
        val reg = Reg(UInt(32.W))
        (reg, reg & delegable_counters)
    }
    val reg_sscratch = Reg(Bits(width = xLen))
    val reg_sepc = Reg(UInt(width = vaddrBits))
    val reg_scause = Reg(Bits(width = xLen))
    val reg_stval = Reg(Bits(width = xLen))
    val reg_satp = Reg(new Satp)
    io.out.satp := reg_satp
    val reg_wfi = RegInit(false.B)
    val reg_pmp = Reg(Vec(lumiaParams.nPMPs, new PMPReg))
    val supervisor_read_mapping = LinkedHashMap[Int, Bits] (
        CSRs.sie -> read_sie,
        CSRs.sip -> read_sip,
        CSRs.sstatus -> read_sstatus,
        CSRs.stvec -> read_stvec,
        CSRs.scounteren -> read_scounteren,
        CSRs.sscratch -> reg_sscratch,
        CSRs.scause -> reg_scause,
        CSRs.stval -> reg_stval(xLen - 1, 0),
        CSRs.satp -> reg_satp.asUInt(),
        CSRs.sepc -> readEPC(reg_sepc.asUInt())(vaddrBits - 1, 0)
    )
    //  Others
    val set_fs_dirty = Wire(init = io.in.set_fs_dirty)
    val reg_fflags = Reg(UInt(width = 5))
    val reg_frm = Reg(UInt(width = 3))
    io.out.fcsr_frm := reg_frm
    when (io.in.fcsr_fflags.valid) {
        reg_fflags := reg_fflags | io.in.fcsr_fflags.bits
        set_fs_dirty := true.B
    }
    if (lumiaParams.haveFSDirty) {
        when (set_fs_dirty) {
            reg_mstatus.fs := 3
        }
    }

    val read_fcsr = Cat(0.U(24.W), reg_frm, reg_fflags)
    val reset_dcsr = Wire(init = new DCSR().fromBits(0))
    reset_dcsr.xdebugver := 1
    reset_dcsr.prv := PRV.M
    val reg_dcsr = Reg(init = reset_dcsr)
    val reg_dpc = Reg(UInt(width = vaddrBits))
    val reg_dscratch = Reg(UInt(width = xLen))
    val misc_read_mapping = LinkedHashMap[Int, Bits] (
        CSRs.fflags -> reg_fflags,
        CSRs.frm -> reg_frm,
        CSRs.fcsr -> read_fcsr,
        CSRs.dcsr -> reg_dcsr.asUInt(),
        CSRs.dpc -> readEPC(reg_dpc)(xLen - 1, 0),
        CSRs.dscratch -> reg_dscratch
    )
    val read_mapping = machine_read_mapping ++ supervisor_read_mapping ++ misc_read_mapping
    //  Processing exceptions
    val insn_call = io.in.ecall
    val insn_break = io.in.ebreak
    val insn_ret = io.in.mret | io.in.sret | io.in.uret | io.in.dret
    val insn_wfi = io.in.wfi
    val insn_sfence = io.in.sfence
    //
    val cause = Mux(insn_call, reg_mstatus.mprv + Causes.user_ecall, Mux[UInt](insn_break, Causes.breakpoint, io.in.exc_cause))
    val cause_lsbs = cause(cause.getWidth - 1, 0)
    val reg_singleStepped = Reg(Bool())
    val reg_debug = Reg(init = false.B)
    val causeIsDebugInt = cause(xLen - 1) && cause_lsbs === CSR.debugTriggerCause
    val causeIsDebugTrigger = !cause(xLen - 1) && cause_lsbs === CSR.debugTriggerCause
    val causeIsDebugBreak = !cause(xLen - 1) && insn_break && Cat(reg_dcsr.ebreakm, reg_dcsr.ebreakh, reg_dcsr.ebreaks, reg_dcsr.ebreaku)(reg_mstatus.mprv)
    val trapToDebug = reg_singleStepped | causeIsDebugInt | causeIsDebugTrigger | causeIsDebugBreak | reg_debug
    val debugTVec = Mux(reg_debug, Mux(insn_break, UInt(0x800), UInt(0x808)), UInt(0x800))
    //  Delegation
    val delegate = reg_mstatus.mprv <= PRV.S && Mux(cause(xLen - 1), read_mideleg(cause_lsbs), read_medeleg(cause_lsbs))
    val notDebugTVec = {
        val base = Mux(delegate, read_stvec, read_mtvec)
        val interrupteOffset = cause(log2Ceil(xLen) - 1, 0) << 2
        val interruptVec = Cat(base >> (log2Ceil(xLen) + 2), interrupteOffset)
        val doVector = base(0) && cause(cause.getWidth - 1) && (cause_lsbs >> log2Ceil(xLen)).asUInt() === 0.U
        Mux(doVector, interruptVec, base >> 2 << 2)
    }
    val epc = readEPC(io.in.exc_pc)
    val exception = insn_call || insn_break || io.in.exception

    when (insn_wfi && !io.out.singleStep && !reg_debug) { reg_wfi := true.B}
    when (pending_interrupts.orR() || io.in.interrupts.debug || exception) { reg_wfi := false.B }
    when (io.in.retire(0) || exception) { reg_singleStepped := true.B }
    when (!io.out.singleStep) { reg_singleStepped := false.B }

    val tvec = Mux(trapToDebug, debugTVec, notDebugTVec)
    io.out.trap_addr := tvec

    when (exception) {
        io.out.trap_kill := true.B
        when (trapToDebug) {
            when (!reg_debug) {
                reg_debug := true.B
                reg_dpc := epc
                reg_dcsr.cause := Mux(reg_singleStepped, 4, Mux(causeIsDebugInt, 3, Mux[UInt](causeIsDebugTrigger, 2, 1)))
                reg_dcsr.prv := reg_mstatus.mprv
                new_prv := PRV.M
            }
        } .elsewhen (delegate) {
            reg_sepc := epc
            reg_scause := cause
            reg_stval := io.in.badvaddr
            reg_mstatus.spie := reg_mstatus.sie
            reg_mstatus.spp := reg_mstatus.mprv
            reg_mstatus.sie := false.B
            new_prv := PRV.S
        } .otherwise {
            reg_mepc := epc
            reg_mcause := cause
            reg_mtval := io.in.badvaddr
            reg_mstatus.mpie := reg_mstatus.mie
            reg_mstatus.mpp := reg_mstatus.mprv
            reg_mstatus.mie := false.B
            new_prv := PRV.M
        }
    } .otherwise { io.out.trap_kill := false.B }

    when (insn_ret) {
        when (!io.rw.addr(9)) {
            reg_mstatus.sie := reg_mstatus.spie
            reg_mstatus.spie := true.B
            reg_mstatus.spp := PRV.U
            new_prv := reg_mstatus.spp
            io.out.trap_kill := true.B
            io.out.trap_addr := readEPC(reg_sepc)
        } .elsewhen (io.rw.addr(10)) {
            new_prv := reg_dcsr.prv
            reg_debug := false.B
            io.out.trap_kill := true.B
            io.out.trap_addr := readEPC(reg_dpc)
        } .otherwise {
            reg_mstatus.mie := reg_mstatus.mpie
            reg_mstatus.mpie := true.B
            reg_mstatus.mpp := PRV.U
            new_prv := reg_mstatus.mpp
            io.out.trap_kill := true.B
            io.out.trap_addr := readEPC(reg_mepc)
        }
    } .otherwise { io.out.trap_kill := false.B }
    //  Read & Write
    val decoded_addr = read_mapping map { case (k, v) => k -> (io.rw.addr === k) }
    io.rw.rdata := Mux1H(for ((k, v) <- read_mapping) yield decoded_addr(k) -> v)
    val csr_wren = io.rw.wren
    when (csr_wren) {
        when (decoded_addr(CSRs.mstatus)) {
            val new_mstatus = new MStatus().fromBits(io.rw.wdata)
            reg_mstatus.mie := new_mstatus.mie
            reg_mstatus.mpie := new_mstatus.mpie
            reg_mstatus.mprv := new_mstatus.mprv
            reg_mstatus.mxr := new_mstatus.mxr
            reg_mstatus.sum := new_mstatus.sum
            reg_mstatus.spp := new_mstatus.spp
            reg_mstatus.spie := new_mstatus.spie
            reg_mstatus.sie := new_mstatus.sie
            reg_mstatus.tw := new_mstatus.tw
            reg_mstatus.tvm := new_mstatus.tvm
            reg_mstatus.tsr := new_mstatus.tsr
        }
        when (decoded_addr(CSRs.mip)) {
            val new_mip = new MIP().fromBits(io.rw.wdata)
            reg_mip.ssip := new_mip.ssip
            reg_mip.stip := new_mip.stip
            reg_mip.seip := new_mip.seip
        }
        when (decoded_addr(CSRs.mie))       { reg_mie := io.rw.wdata & supported_interrupts }
        when (decoded_addr(CSRs.mepc))      { reg_mepc := formEPC(io.rw.wdata)}
        when (decoded_addr(CSRs.mscratch))  { reg_mscratch := io.rw.wdata }
        when (decoded_addr(CSRs.mtvec))     { reg_mtvec := io.rw.wdata }
        when (decoded_addr(CSRs.mcause))    { reg_mcause := UInt((BigInt(1) << (xLen - 1)) + (BigInt(1) << whichInterrupt.getWidth) - 1) }
        when (decoded_addr(CSRs.mtval))     { reg_mtval := io.rw.wdata(vaddrBits - 1, 0)}
        writeCounter(CSRs.mcycle, reg_cycle, io.rw.wdata)
        writeCounter(CSRs.minstret, reg_instret, io.rw.wdata)
        when (decoded_addr(CSRs.fflags)) { reg_fflags := io.rw.wdata }
        //  Float
        when (decoded_addr(CSRs.frm))   { reg_frm := io.rw.wdata }
        when (decoded_addr(CSRs.fcsr)) {
            set_fs_dirty := true.B
            reg_fflags := io.rw.wdata
            reg_frm := io.rw.wdata >> reg_fflags.getWidth
        }
        //  Debug
        when (decoded_addr(CSRs.dcsr)) {
            val new_dcsr = new DCSR().fromBits(io.rw.wdata)
            reg_dcsr.step := new_dcsr.step
            reg_dcsr.ebreakm := new_dcsr.ebreakm
            reg_dcsr.ebreaks := new_dcsr.ebreaks
            reg_dcsr.ebreaku := new_dcsr.ebreaku
            reg_dcsr.prv := new_dcsr.prv
        }
        when (decoded_addr(CSRs.dpc))       { reg_dpc := formEPC(io.rw.wdata) }
        when (decoded_addr(CSRs.dscratch))  { reg_dscratch := io.rw.wdata }
        //  Supervisor
        when (decoded_addr(CSRs.sstatus)) {
            val new_sstatus = new MStatus().fromBits(io.rw.wdata)
            reg_mstatus.sie := new_sstatus.sie
            reg_mstatus.spie := new_sstatus.spie
            reg_mstatus.spp := new_sstatus.spp
            reg_mstatus.mxr := new_sstatus.mxr
            reg_mstatus.sum := new_sstatus.sum
            reg_mstatus.fs := new_sstatus.fs
            reg_mstatus.vs := 0.U
        }
        when (decoded_addr(CSRs.sip)) {
            val new_sip = new MIP().fromBits((read_mip & (~read_mideleg).asUInt()) | (io.rw.wdata & read_mideleg))
            reg_mip.ssip := new_sip.ssip
        }
        when (decoded_addr(CSRs.satp)) {
            val new_satp = new Satp().fromBits(io.rw.wdata)
            reg_satp.mode := new_satp.mode
            reg_satp.ppn := new_satp.ppn(ppnBits - 1, 0)
            reg_satp.asid := new_satp.asid(asidBits - 1, 0)
        }
        when (decoded_addr(CSRs.sie))           { reg_mie := (reg_mie & (~read_mideleg).asUInt()) | (io.rw.wdata & read_mideleg)}
        when (decoded_addr(CSRs.sscratch))      { reg_sscratch := io.rw.wdata }
        when (decoded_addr(CSRs.sepc))          { reg_sepc := formEPC(io.rw.wdata) }
        when (decoded_addr(CSRs.stvec))         { reg_stvec := io.rw.wdata }
        when (decoded_addr(CSRs.scause))        { reg_scause := io.rw.wdata & UInt((BigInt(1) << (xLen - 1)) + 31) }
        when (decoded_addr(CSRs.stval))         { reg_stval := io.rw.wdata(vaddrBits - 1, 0) }
        when (decoded_addr(CSRs.mideleg))       { reg_mideleg := io.rw.wdata }
        when (decoded_addr(CSRs.medeleg))       { reg_medeleg := io.rw.wdata }
        when (decoded_addr(CSRs.scounteren))    { reg_scounteren := io.rw.wdata }
        when (decoded_addr(CSRs.mcounteren))    { reg_mcounteren := io.rw.wdata }
    }
    //
    def writeCounter(lo: Int, ctr: WideCounter, wdata: UInt) = {
        val hi = lo + CSRs.mcycleh - CSRs.mcycle
        when (decoded_addr(lo)) {ctr := Cat(ctr.value(ctr.getWidth - 1, 32), wdata)}
        when (decoded_addr(hi)) {ctr := Cat(wdata(ctr.getWidth - 33, 0), ctr.value(31, 0))}
    }
    //
    io.out.csr_stall := reg_wfi
    io.out.singleStep := reg_dcsr.step && !reg_debug
}