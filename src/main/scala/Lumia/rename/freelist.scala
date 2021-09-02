package Lumia.rename

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import Lumia.utils._

class FreeListIO(numPRegs: Int)(implicit p: Parameters) extends LumiaBundle {
    val stall = Bool(INPUT)
    val alloc_req = Vec(decodeWidth, Bool(INPUT))
    val fire_req = Vec(retireWidth, Valid(UInt(width = log2Ceil(numPRegs))).flip)
    val alloc_preg = Vec(decodeWidth, UInt(OUTPUT, width = log2Ceil(numPRegs)))
    val empty = Bool(OUTPUT)
}

class FreeList(numPRegs: Int)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new FreeListIO(numPRegs))

    //***************************************
    //  Module body
    val free_list = RegInit(UInt(numPRegs.W), Cat(Fill(numPRegs - 32, 1.U), Fill(32, 0.U)))
    val sels = select_first(free_list, decodeWidth)
    val alloc_mask = sels zip io.alloc_req map { case (f, s) => f & Fill(numPRegs, s)} reduce (_|_)
    val dealloc_mask = io.fire_req.map(s => Fill(numPRegs, s.valid) & UIntToOH(s.bits)).reduce(_|_)
    when (io.stall) {
        free_list := (free_list | dealloc_mask) & ~(1.U(numPRegs.W))
    } .elsewhen (io.alloc_req.reduce(_|_)) {
        free_list := (free_list & ~alloc_mask | dealloc_mask) & ~(1.U(numPRegs.W))
    } .otherwise {
        free_list := (free_list | dealloc_mask) & ~(1.U(numPRegs.W))
    }
    io.alloc_preg := sels.map(OHToUInt(_))
    io.empty := PopCount(io.alloc_req) > PopCount(free_list)
}