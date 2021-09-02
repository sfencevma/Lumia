package Lumia.common

import Chisel._
import freechips.rocketchip.config.Parameters

class Uop(implicit p: Parameters) extends LumiaBundle {
    val len = Bool()
    val rs1_type = UInt(RT_SZ.W)
    val rs1 = UInt(pregSz.W)
    val rs2_type = UInt(RT_SZ.W)
    val rs2 = UInt(pregSz.W)
    val rs3_type = UInt(RT_SZ.W)
    val rs3 = UInt(pregSz.W)
    val rd_type = UInt(RT_SZ.W)
    val rd = UInt(pregSz.W)
    val imm_sel = UInt(IF_SZ.W)
    val imm_package = UInt(LONG_IMM_SZ.W)
    val sign_op = Bool()
    val fu = UInt(FU_SZ.W)
    val uop = UInt(uopBits.W)
    val mem_size = UInt(MEM_SZ.W)
}

trait SpecialUops  {
    def spec_uop(op: UInt)(implicit p: Parameters): Uop = {
        val uop = Wire(new Uop)
        uop := chisel3.DontCare
        uop.len := LEN_32
        uop.rs1_type := RT_NON
        uop.rs1 := 0.U
        uop.rs2_type := RT_NON
        uop.rs2 := 0.U
        uop.rs3_type := RT_NON
        uop.rs3 := 0.U
        uop.rd_type := RT_NON
        uop.rd := 0.U
        uop.imm_sel := IF_NON
        uop.imm_package := 0.U
        uop.sign_op := 0.U
        uop.fu := FU_ALU
        uop.uop := op
        uop.mem_size := MEM_NON
        uop
    }

}