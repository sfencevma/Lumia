package Lumia.rename

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import Lumia.iq._
import Lumia.decode._

class RenameReq(implicit p: Parameters) extends LumiaBundle {
    val lreg = UInt(INPUT, width = lregSz)
    val preg = UInt(OUTPUT, width = pregSz)
}
class RenameResp(implicit p: Parameters) extends DecodeResp {
    val gc_tag      = UInt(width = gcTagSz)
}

class RenameModuleIO(implicit p: Parameters) extends LumiaBundle {
    val kill = new Bundle() {
        val trap_kill = Valid(UInt(width = gcTagSz)).flip
        val bjp_kill = Valid(UInt(width = gcTagSz)).flip
    }
    val stall   = Bool(INPUT)
    val req     = Vec(decodeWidth, Valid(new DecodeResp).flip)
    val rs1     = Vec(decodeWidth, new RenameReq)
    val rs2     = Vec(decodeWidth, new RenameReq)
    val rs3     = Vec(decodeWidth, new RenameReq)
    val rd      = Vec(decodeWidth, new RenameReq)
    val rd_type = Vec(decodeWidth, Valid(UInt(width = RT_SZ)).flip)
    val fire_req = Vec(retireWidth, Valid(new Bundle {
        val gc_tag = UInt(width = gcTagSz)
        val prd = UInt(width = pregSz)
        val rd_type = UInt(width = RT_SZ)
    }).flip)

    val resp    = Vec(decodeWidth, Valid(new RenameResp))
    val gc_tag  = Vec(decodeWidth, UInt(OUTPUT, width = gcTagSz))
    val ren_stall = Bool(OUTPUT)
}


class RenameModule(numRegs: Int, numPRegs: Int, numCheckPoints: Int, float: Boolean)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new RenameModuleIO)
    
    //***********************************************
    //  Module body
    val rtype = if (float) RT_FLT else RT_REG
    val alloc_req = io.rd_type.map(s => s.valid && s.bits === rtype)
    val fire_req = io.fire_req.map(s => s.valid && s.bits.rd_type === rtype)
    val free_list = Module(new FreeList(numPRegs)).io
    val maptable = Module(new MapTable(numRegs, numPRegs, numCheckPoints)).io
    //  Free list
    free_list.stall := io.stall || maptable.empty
    free_list.alloc_req := alloc_req
    free_list.fire_req.zipWithIndex zip fire_req map { case ((f, i), s) => f.valid := s; f.bits := io.fire_req(i).bits.prd }

    //  Mapping table
    maptable.kill.trap_kill := io.kill.trap_kill
    maptable.kill.bjp_kill := io.kill.bjp_kill
    maptable.stall := io.stall || free_list.stall
    maptable.ren_req := io.rd_type.map(_.valid)
    maptable.rs1.zipWithIndex.foreach{ case (rs, i) => rs := io.rs1(i).lreg }
    maptable.rs2.zipWithIndex.foreach{ case (rs, i) => rs := io.rs2(i).lreg }
    maptable.rs3.zipWithIndex.foreach{ case (rs, i) => rs := io.rs3(i).lreg }
    maptable.rd.zipWithIndex.foreach{ case (rs, i) => rs := io.rd(i).lreg }
    maptable.alloc_req.zipWithIndex zip free_list.alloc_preg map { case ((f, i), s) => f.valid := alloc_req(i); f.bits := s}

    //  Fire
    maptable.fire_req.zipWithIndex zip fire_req map { case ((f, i), s) => f.valid := s; f.bits := io.fire_req(i).bits.gc_tag }

    //  Result
    io.rs1 zip maptable.prs1 map { case (f, s) => f.preg := s }
    io.rs2 zip maptable.prs2 map { case (f, s) => f.preg := s }
    io.rs3 zip maptable.prs3 map { case (f, s) => f.preg := s }
    io.rd zip free_list.alloc_preg map { case (f, s) => f.preg := s }
    io.gc_tag := maptable.gc_tag

    io.resp := chisel3.DontCare
    io.ren_stall := io.stall || free_list.empty || maptable.empty
}

class RenameStage(implicit p: Parameters) extends LumiaModule {
    val io = IO(new RenameModuleIO)

    //*********************************************
    //  Module body
    val kill = io.kill.trap_kill.valid || io.kill.bjp_kill.valid
    //
    val rename = Module(new RenameModule(numLogicalRegisters, numIntPhysRegisters, numIntCheckPoints, false)).io
    val fp_rename = if (usingFPU) Module(new RenameModule(numLogicalRegisters, numFpPhysRegisters, numFpCheckPoints, true)).io else null
    val rename_stages = if (usingFPU) Seq(rename, fp_rename) else Seq(rename)


    for (rename <- rename_stages) {
        rename.kill.trap_kill := io.kill.trap_kill
        rename.kill.bjp_kill := io.kill.bjp_kill
        rename.stall := io.stall
        rename.rs1 zip io.req.map(_.bits.uop.rs1) map { case (f, s) => f.lreg := s(lregSz - 1, 0) }
        rename.rs2 zip io.req.map(_.bits.uop.rs2) map { case (f, s) => f.lreg := s(lregSz - 1, 0) }
        rename.rs3 zip io.req.map(_.bits.uop.rs3) map { case (f, s) => f.lreg := s(lregSz - 1, 0) }
        rename.rd zip io.req.map(_.bits.uop.rd) map { case (f, s) => f.lreg := s(lregSz - 1, 0) }
        rename.rd_type zip io.req map { case (f, s) => f.valid := s.valid; f.bits := s.bits.uop.rd_type }
        rename.fire_req := io.fire_req
    }

    val fp_inst = io.req.map(s => if (usingFPU) s.bits.uop.fu === FU_FPU else false.B)
    val prs1 = rename_stages.map(_.rs1.map(_.preg))
    val prs2 = rename_stages.map(_.rs2.map(_.preg))
    val prs3 = rename_stages.map(_.rs3.map(_.preg))
    val prd = rename_stages.map(_.rd.map(_.preg))
    val gc_tag = rename_stages.map(_.gc_tag)

    val ren_resp = Array.fill(decodeWidth){Reg(Valid(new RenameResp))}
    for (dw <- 0 until decodeWidth) {
        when (kill) { ren_resp(dw).valid := false.B }
          .elsewhen (io.req(dw).valid) {
              //    Common
              ren_resp(dw).valid := true.B
              ren_resp(dw).bits.addr := io.req(dw).bits.addr
              ren_resp(dw).bits.taddr := io.req(dw).bits.taddr
              ren_resp(dw).bits.len := io.req(dw).bits.len
              ren_resp(dw).bits.cause := io.req(dw).bits.cause
              ren_resp(dw).bits.s2_bpu_pred := io.req(dw).bits.s2_bpu_pred
              //    Set uop
              ren_resp(dw).bits.uop := io.req(dw).bits.uop
              ren_resp(dw).bits.uop.rs1 := Mux(fp_inst(dw), prs1(1)(0), prs1.head(0))
              ren_resp(dw).bits.uop.rs2 := Mux(fp_inst(dw), prs2(1)(1), prs2.head(1))
              ren_resp(dw).bits.uop.rs3 := Mux(fp_inst(dw), prs3(1)(2), prs1.head(2))
              ren_resp(dw).bits.uop.rd  := Mux(fp_inst(dw), prd(1)(3), prd.head(3))

              //
              ren_resp(dw).bits.gc_tag := Mux(fp_inst(dw), gc_tag(1)(dw), gc_tag(0)(dw))
          } .otherwise { ren_resp(dw).valid := false.B }
    }
    //
    io.resp <> ren_resp
    io.ren_stall := rename_stages.map(_.ren_stall).reduce(_|_)
}

