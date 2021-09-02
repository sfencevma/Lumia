package Lumia.rename

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import Lumia.utils._

class TableSlotIO(numRegs: Int, numReadPorts: Int)(implicit p: Parameters) extends LumiaBundle {
    val tag = Vec(numReadPorts, UInt(INPUT, width = log2Ceil(numRegs)))
    val alloc_req = Valid(UInt(width = log2Ceil(numRegs))).flip
    val clear = Vec(decodeWidth, Valid(UInt(width = log2Ceil(numRegs))).flip)
    val rollback = Valid(Bool()).flip
    val valid = Vec(numReadPorts, Bool(OUTPUT))
    val gc = Bool(OUTPUT)
}

class TableSlot(init: Int, default: Bool, numRegs: Int, numReadPorts: Int)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new TableSlotIO(numRegs, numReadPorts))
    val valid = RegInit(default)
    val tag = RegInit(init.U(log2Ceil(numRegs).W))
    val need_clear = io.clear.map(s => s.valid & s.bits === tag) reduce (_|_)

    when (io.rollback.valid) { valid := io.rollback.bits }
      .elsewhen (io.alloc_req.valid) {
          valid := true.B
          tag := io.alloc_req.bits
      } .elsewhen (need_clear) { valid := false.B }
    io.valid := io.tag.map(_ === tag & valid)
    io.gc := valid
}

class MapTableIO(numRegs: Int, numPRegs: Int)(implicit p: Parameters) extends LumiaBundle {
    val kill = new Bundle() {
        val trap_kill = Valid(UInt(width = gcTagSz)).flip
        val bjp_kill = Valid(UInt(width = gcTagSz)).flip
    }
    val stall = Input(Bool())
    //  Require rename
    val ren_req = Vec(decodeWidth, Bool(INPUT))
    val rs1 = Vec(decodeWidth, UInt(INPUT, width = log2Ceil(numRegs)))
    val rs2 = Vec(decodeWidth, UInt(INPUT, width = log2Ceil(numRegs)))
    val rs3 = Vec(decodeWidth, UInt(INPUT, width = log2Ceil(numRegs)))
    val rd  = Vec(decodeWidth, UInt(INPUT, width = log2Ceil(numRegs)))
    //  Allocata rd preg
    val alloc_req = Vec(decodeWidth, Valid(UInt(width = log2Ceil(numPRegs))).flip)
    //  Retire
    val fire_req = Vec(decodeWidth, Valid(UInt(width = gcTagSz)).flip)

    //  Result
    val prs1 = Vec(decodeWidth, UInt(OUTPUT, width = log2Ceil(numPRegs)))
    val prs2 = Vec(decodeWidth, UInt(OUTPUT, width = log2Ceil(numPRegs)))
    val prs3 = Vec(decodeWidth, UInt(OUTPUT, width = log2Ceil(numPRegs)))
    //
    val gc_tag = Vec(decodeWidth, UInt(OUTPUT, width = gcTagSz.W))
    val empty = Bool(OUTPUT)
}


abstract class AbstractMapTable(numRegs: Int, numPRegs: Int, numCheckPoints: Int)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new MapTableIO(numRegs, numPRegs))
    val slots = ((for (s <- 0 until numRegs)
        yield
        {val slot = Module(new TableSlot(s, true.B, numRegs, 4 * decodeWidth)); slot})
      ++
      (for (s <- 0 until numPRegs - numRegs)
          yield
          {val slot = Module(new TableSlot(0, false.B, numRegs, 4 * decodeWidth)); slot}))
    val maptable = Vec(slots.map(_.io))
    val gc = Reg(Vec(numCheckPoints, UInt(numPRegs.W)))
    val ren_req = io.rs1 ++ io.rs2 ++ io.rs3 ++ io.rd
    for (r <- 0 until numPRegs) {
        for (p <- 0 until 4 * decodeWidth) {
            maptable(r).tag(p) := ren_req(p)(lregSz - 1, 0)
        }
        maptable(r).alloc_req.valid := io.alloc_req.map(s => s.valid & s.bits === r.U) reduce (_|_)
        maptable(r).alloc_req.bits := io.alloc_req zip io.rd map { case (f, s) => Fill(log2Ceil(numRegs), r.U === f.bits) & s} reduce (_|_)
    }
    for (slot <- maptable) {
        (slot.clear zip io.alloc_req) zip io.rd map {
            case ((f, s), t) => {
                f.valid := s.valid
                f.bits := t
            }
        }
    }
    val free_list = RegInit(UInt(numCheckPoints.W), ~0.U(numCheckPoints.W))
}


class MapTable(numRegs: Int, numPRegs: Int, numCheckPoints: Int)(implicit p: Parameters) extends AbstractMapTable(numRegs, numPRegs, numCheckPoints) {
    //*********************************************************
    //  Module body
    val kill = io.kill.trap_kill.valid || io.kill.bjp_kill.valid
    val need_map = io.ren_req.reduce(_|_)
    val need_ret = io.fire_req.map(_.valid).reduce(_|_)
    //  Mapping
    io.prs1 := (for (i <- 0                 until   decodeWidth) yield {maptable.map(_.valid).map(_(i))}).map(OHToUInt(_))
    io.prs2 := (for (i <- decodeWidth     until 2 * decodeWidth) yield {maptable.map(_.valid).map(_(i))}).map(OHToUInt(_))
    io.prs3 := (for (i <- 2 * decodeWidth until 3 * decodeWidth) yield {maptable.map(_.valid).map(_(i))}).map(OHToUInt(_))
    //
    val kill_tag = Mux(io.kill.trap_kill.valid, io.kill.trap_kill.bits, io.kill.bjp_kill.bits)
    for (s <- 0 until numPRegs) {
        maptable(s).rollback.valid := kill
        maptable(s).rollback.bits := gc(kill_tag)(s)
    }
    val sels = select_first(free_list, decodeWidth)
    val alloc_mask = sels zip io.alloc_req map { case (f, s) => f & Fill(numCheckPoints, s.valid) } reduce (_|_)
    val dealloc_mask = (io.fire_req.map(s => Fill(numCheckPoints, s.valid) & UIntToOH(s.bits)).reduce (_|_)
      | (Fill(numCheckPoints, io.kill.bjp_kill.valid) & UIntToOH(io.kill.bjp_kill.bits))
      | (Fill(numCheckPoints, io.kill.trap_kill.valid) & UIntToOH(io.kill.trap_kill.bits)))
    when (io.stall || kill) {
        free_list := free_list | dealloc_mask
    } .elsewhen (need_ret || need_map) {
        free_list := free_list & ~alloc_mask | dealloc_mask
    }
    io.gc_tag := sels.map(OHToUInt(_))
    //  Clear
    val clear = Vec(Array.fill(decodeWidth){0.U(numPRegs.W)})
    clear(0) := 0.U
    for (dw <- 1 until decodeWidth) {
        var mask = 0.U(numPRegs.W)
        for (j <- 0 until dw) {
            mask |= Vec(maptable.map(_.valid).map(_(3 * decodeWidth + j))).asUInt() & Fill(numPRegs, io.alloc_req(j).valid)
        }
        clear(dw) := mask
    }
    //  Set
    val set = Vec(Array.fill(decodeWidth){0.U(numPRegs.W)})
    set(0) := 0.U
    for (dw <- 1 until decodeWidth) {
        var mask = 0.U(numPRegs.W)
        for (j <- 0 until dw) {
            mask |= UIntToOH(io.alloc_req(j).bits) & Fill(numPRegs, io.alloc_req(j).valid)
        }
        set(dw) := mask
    }
    for (dw <- 0 until decodeWidth) {
        when (io.ren_req(dw)) {
            gc(io.gc_tag(dw)) := Vec(maptable.map(_.gc)).asUInt() & ~clear(dw) | set(dw)
        }
    }
    io.empty := PopCount(io.ren_req) > PopCount(free_list)
}