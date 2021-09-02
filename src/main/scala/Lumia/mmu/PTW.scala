package Lumia.mmu

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.rocket.Causes
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import Lumia.devices.axi4._
import Lumia.common._
import Lumia.utils._
import Lumia.cache._
import Lumia.exu._

class PTWReq(implicit p: Parameters) extends LumiaBundle with MemoryOpConstants {
    val addr = UInt(width = vaddrBits)
    val cmd  = UInt(width = M_SZ)
}

class PTWResp(implicit p: Parameters) extends LumiaBundle {
    val pte = new PTE
    val exc_val = Bool()
    val cause = UInt(width = xLen)
}

class PTWIO(implicit p: Parameters) extends LumiaBundle {
    val req = Decoupled(new PTWReq)
    val resp = Valid(new PTWResp).flip
}

class CachePTWReq(implicit p: Parameters) extends LumiaBundle with MemoryOpConstants {
    val addr = UInt(width = paddrBits)
    val cmd  = UInt(width = M_SZ)
}

class CachePTWResp(implicit p: Parameters) extends LumiaBundle {
    val data = UInt(width = 512)
    val exc_val = Bool()
    val cause = UInt(width = xLen)
}

class CachePTWIO(implicit p: Parameters) extends LumiaBundle {
    val req = Decoupled(new CachePTWReq)
    val resp = Valid(new CachePTWResp).flip
}

class DatapatchPTWIO(implicit p: Parameters) extends LumiaBundle {
    val sfence = Valid(new SFenceReq).flip
    val status = new MStatus().asInput
    val satp = new Satp().asInput
    val pmp = Vec(nPMPs, new PMP).asInput
}

class PTW(params: AXI4BundleParameters, nRequests: Int)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new Bundle {
        val axi = new AXI4Bundle(params)
        val requestor = Vec(nRequests, new PTWIO)
        val icache_req = new CachePTWIO
        val dcache_req = new CachePTWIO
        val wt = DecoupledIO(new CacheWriteData(512)).flip
        val dpath = new DatapatchPTWIO
        val kill = Bool(INPUT)
    })


    val s_ready::s_req::s_ar::s_r::s_walking::s_update::Nil = Enum(6)
    val state = Reg(init = s_ready)


    //************************************************
    //  RR arbiter
    val rr_arb = Module(new RRArbiter(new PTWReq, nRequests))
    rr_arb.io.in <> io.requestor.map(_.req)
    rr_arb.io.out.ready := state === s_ready

    val r_req = Reg(new PTWReq)
    val r_req_dest = Reg(Bits())

    when (rr_arb.io.out.fire()) {
        r_req := rr_arb.io.out.bits
        r_req_dest := rr_arb.io.chosen
    }

    //************************************************
    //  L2TLB
    val l2tlb = new L2TLB().module.io
    l2tlb.satp   := io.dpath.satp
    l2tlb.prv    := io.dpath.status.prv
    l2tlb.kill   := io.kill
    l2tlb.sfence := io.dpath.sfence

    l2tlb.req.valid := rr_arb.io.out.valid
    l2tlb.req.bits  := rr_arb.io.out.bits

    //************************************************
    //  Translation
    val invalidated = Reg(Bool(false))
    val level = Reg(UInt(width = log2Up(pgLevels)))
    val r_pte = Reg(new PTE)

    val pte = new PTE().fromBits(io.axi.r.bits.data)
    val pte_addr = if (!usingVM) {
        UInt(0, width = paddrBits)
    } else {
        val vpn_idxs = (0 until pgLevels).map (i => (r_req.addr >> (pgLevels - i - 1) * pgLevelBits)(pgLevelBits - 1, 0))
        val vpn_idx = Vec(vpn_idxs)(level)
        Cat(r_pte.ppn, vpn_idx) << log2Ceil(xLen / 8)
    }

    //  PMP check
    val lgMaxSize = log2Up(memDataBytes)
    val pmpChecker = Module(new PMPChecker(lgMaxSize))
    val prv = io.dpath.status.prv
    pmpChecker.io.prv := prv
    pmpChecker.io.pmp := io.dpath.pmp
    pmpChecker.io.addr := pte_addr
    pmpChecker.io.size := lgMaxSize.U

    def makePTE(e: TLBEntry) = {
        val pte = Wire(new PTE)
        pte.v := true.B
        pte.r := e.r
        pte.w := e.w
        pte.x := e.x
        pte.u := e.u
        pte.g := e.g
        pte.a := e.a
        pte.d := e.d
        pte.rsw := 0.U
        pte.ppn := e.ppn
        pte
    }

    def makePTE(ppn: UInt, default: PTE) = {
        val pte = Wire(init = default)
        pte.ppn := ppn
        pte
    }

    r_pte := Mux(io.axi.r.fire(), pte,
        Mux(l2tlb.resp.hit, makePTE(l2tlb.resp.entry),
            Mux(rr_arb.io.out.fire(), makePTE(io.dpath.satp.ppn, r_pte), r_pte)))
    val resp_valid = Reg(next = Vec.fill(nRequests)(Bool(false)))

    def isAtomic() = {
        false.B
    }
    def isRead() = {
        false.B
    }
    def isWrite() = {
        false.B
    }

    val cmd_read = isRead()
    val cmd_write = isWrite()
    val cmd_inst = false.B

    //************************************************
    //  Access fault
    val pma_error = Reg(Bool(false))
    val fetch_access_fault = cmd_inst && !pmpChecker.io.x
    val load_access_fault = cmd_read && !pmpChecker.io.r
    val store_access_fault = cmd_write && !pmpChecker.io.w
    val pmp_error = fetch_access_fault || load_access_fault || store_access_fault
    val access_fault = pma_error || pmp_error

    //************************************************
    //  Page fault
    def split_ppn(pte: PTE) = {
        val ppn = pte.ppn
        val ppnVec = if (usingSv32) {
            Vec(Seq(ppn(19, 10), ppn(31,20)))
        } else if (usingSv39) {
            Vec(Seq(ppn(18, 10), ppn(27, 19), ppn(53, 28)))
        } else /* Sv42 */ {
            Vec(Seq(ppn(18, 10), ppn(27, 19), ppn(36, 28), ppn(53, 27)))
        }
        ppnVec
    }

    val pte_is_table = false.B
    val pte_is_leaf = r_pte.v && (r_pte.r || r_pte.x)
    val pte_invalid = !r_pte.v || (!r_pte.r && r_pte.w)
    val pte_misalign = level === 0.U && pte_is_table

    val pte_page_fault = pte_is_leaf && (!r_pte.a || (!r_pte.d && cmd_write))
    val superpage_fault = if (!usingSuperpage) {
        false.B
    } else {
        level > 0.U && split_ppn(r_pte)(level - 1.U).orR()
    }

    val user_page_fault = prv === PRV.U && !r_pte.u
    val supervisor_page_fault = prv === PRV.S && r_pte.u && !io.dpath.status.sum
    val can_not_read = !r_pte.r && cmd_read
    val can_not_write = !r_pte.w && cmd_write
    val can_not_exec = !r_pte.x && cmd_inst
    val prv_page_fault = pte_is_leaf && (can_not_read || can_not_write || can_not_exec || user_page_fault || supervisor_page_fault)
    val page_fault = pte_invalid || pte_misalign || pte_page_fault || superpage_fault || prv_page_fault

    val exc_val = access_fault || page_fault
    val cause = Mux(access_fault,
        Mux(cmd_read, Causes.load_access.U,
            Mux(cmd_write, Causes.store_access.U, Causes.fetch_access.U)),
        Mux(cmd_read, Causes.load_page_fault.U,
            Mux(cmd_write, Causes.store_page_fault.U, Causes.fetch_page_fault.U)))
    //************************************************
    //  Resp
    for (i <- 0 until nRequests) {
        io.requestor(i).resp.valid          := resp_valid(i)
        io.requestor(i).resp.bits.exc_val   := exc_val
        io.requestor(i).resp.bits.cause     := cause
        io.requestor(i).resp.bits.pte       := r_pte
    }

    //************************************************
    //  Update PTE
    val newest_pte = Reg(new PTE)
    newest_pte := r_pte
    newest_pte.a := true.B
    newest_pte.d := Mux(cmd_write, true.B, r_pte.d)

    //************************************************
    //  Load arbiter
    val pte_req = Wire(DecoupledIO(new CachePTWReq).flip)
    pte_req.valid := rr_arb.io.out.valid
    pte_req.bits.addr := pte_addr
    pte_req.bits.cmd := 0.U

    val load_arb = Module(new Arbiter(new CachePTWReq, 3))
    load_arb.io.in(0) <> pte_req
    load_arb.io.in(1) <> io.icache_req
    load_arb.io.in(2) <> io.dcache_req

    //************************************************
    //  Mem arbiter
    val mem_arb = Module(new RRArbiter(Bool(), 2))

    val wt_mem_req = Wire(DecoupledIO(Bool()).flip)
    val arb_mem_req = Wire(DecoupledIO(Bool()).flip)

    wt_mem_req.valid    := io.wt.valid
    wt_mem_req.bits     := io.wt.valid
    arb_mem_req.valid   := load_arb.io.out.valid
    arb_mem_req.bits    := load_arb.io.out.valid

    mem_arb.io.in(0) <> wt_mem_req
    mem_arb.io.in(1) <> arb_mem_req

    load_arb.io.out.ready := mem_arb.io.out.ready
    mem_arb.io.out.ready := io.axi.aw.ready && io.axi.ar.ready

    val mem_addr = Mux(isOneOf(Seq(s_req, s_ar, s_update), state), pte_addr,
        Mux(mem_arb.io.chosen === 0.U, io.wt.bits.addr, load_arb.io.out.bits.addr))
    val pte_bits = newest_pte.getWidth
    val mem_data_bits = io.wt.bits.data.getWidth
    val mem_data = Mux(state === s_update,
        Cat(UInt(0, width = mem_data_bits - pte_bits),
            newest_pte.asUInt()), io.wt.bits.data)
    val mem_wmask = Wire(UInt(width = mem_data_bits / 8))
    mem_wmask := (UIntToOH((Mux(state === s_update, pte_bits, mem_data_bits) / 8).asUInt(), width = mem_data_bits / 8 + 1) - 1.U)(mem_data_bits / 8, 0)
    //************************************************
    //  PTW finite state machine
    val load_arb_sel_0 = load_arb.io.chosen === 0.U
    val mem_arb_sel_1 = mem_arb.io.chosen === 1.U
    val ptw_arb = load_arb_sel_0 && mem_arb_sel_1
    switch (state) {
        is (s_ready) {
            when (rr_arb.io.out.fire()) {
                level := pgLevels - 1.U
                state := s_req
            }
        }
        is (s_req) {
            when (!l2tlb.resp.hit) {
                state := s_ar
            } .otherwise {
                resp_valid(r_req_dest) := true.B
                state := s_ready
            }
        }
        is (s_ar) {
            when (ptw_arb && io.axi.ar.fire()) {
                state := s_r
            }
        }
        is (s_r) {
            when (ptw_arb && io.axi.r.fire()) {
                pma_error := io.axi.r.bits.resp.orR()
                state := s_walking
            }
        }
        is (s_walking) {
            when (access_fault || page_fault || pte_is_leaf) {
                resp_valid(r_req_dest) := true.B
                when (pte_page_fault) {
                    state := s_update
                } .otherwise {
                    state := s_ready
                }
            } .otherwise {
                when (level === 0.U) {
                    resp_valid(r_req_dest) := true.B
                } .otherwise {
                    level := level - 1.U
                    state := s_ar
                }
            }
        }
        is (s_update) {
            when (ptw_arb && io.axi.b.fire()) {
                state := s_ready
            }
        }
    }

    when (io.kill) { state := s_ready }

    //************************************************
    //  AXI4
    //  Write port
    io.axi.aw.valid         := false.B
    io.axi.aw.bits.addr     := mem_addr
    io.axi.aw.bits.burst    := AXI4Parameters.BURST_INCR
    io.axi.aw.bits.size     := 7.U
    io.axi.aw.bits.len      := 3.U
    io.axi.aw.bits.id       := 0.U  //  DontCare
    io.axi.aw.bits.lock     := 0.U  //  Normal access
    io.axi.aw.bits.cache    := 0.U  //  Noncacheable and nonbufferable
    io.axi.aw.bits.prot     := AXI4Parameters.PROT_INSECURE

    //  Unzip
    val nParts = 512 / memDataBits
    val splitData = Wire(Vec(nParts, UInt(width = memDataBits)))
    (0 until nParts).map(s => {
        splitData(s) := mem_data(8 * s + 127, 8 * s)
    })
    val splitStrb = Wire(Vec(nParts, UInt(width = memDataBytes)))
    (0 until nParts).map(s => {
        splitStrb(s) := mem_wmask(s + 15, s)
    })
    val part_sel = Reg(init = UInt(0, width = log2Ceil(nParts)))

    when (io.kill) {
        part_sel := 0.U
    } .elsewhen (io.axi.b.fire()) {
        part_sel := part_sel + 1.U
    }

    io.axi.w.valid          := HoldControl(io.axi.aw.fire(), io.axi.w.fire())
    io.axi.w.bits.data      := splitData(part_sel)
    io.axi.w.bits.strb      := splitStrb(part_sel)
    io.axi.w.bits.last      := part_sel === nParts - 1.U

    io.axi.b.ready := HoldControl(io.axi.w.fire(), io.axi.b.fire())

    //  Read port
    io.axi.ar.valid     := false.B
    io.axi.ar.bits.id   := 0.U  //  DontCare
    io.axi.ar.bits.addr := mem_addr
    io.axi.ar.bits.len  := 3.U
    io.axi.ar.bits.size := 7.U
    io.axi.ar.bits.burst := AXI4Parameters.BURST_INCR
    io.axi.ar.bits.lock := 0.U
    io.axi.ar.bits.cache:= 0.U
    io.axi.ar.bits.prot := AXI4Parameters.PROT_INSECURE

    io.axi.r.ready := HoldControl(io.axi.ar.fire(), io.axi.r.fire())
}
