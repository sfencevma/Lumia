package Lumia.common

import Chisel._
import freechips.rocketchip.config._
import Lumia.iq.s2_bpu_pred

trait uop {
    val X = BitPat("b?")
    val Y = BitPat("b1")
    val N = BitPat("b0")
    //
    val U = BitPat("b0")
    val S = BitPat("b1")
    //
    val LEN_SZ = 2
    val LEN_X = BitPat.dontCare(LEN_SZ)
    val LEN_16 = 1.U(LEN_SZ.W)
    val LEN_32 = 2.U(LEN_SZ.W)
    //
    val IF_SZ   = 3
    val IF_X    = BitPat.dontCare(IF_SZ)
    val IF_NON  =  0.U(IF_SZ.W)
    val IF_I    =  1.U(IF_SZ.W)
    val IF_S    =  2.U(IF_SZ.W)
    val IF_B    =  3.U(IF_SZ.W)
    val IF_U    =  4.U(IF_SZ.W)
    val IF_J    =  5.U(IF_SZ.W)
    //
    val RT_SZ   = 3
    val RT_X    = BitPat.dontCare(RT_SZ)
    val RT_NON  = 0.U(RT_SZ.W)
    val RT_REG  = 1.U(RT_SZ.W)
    val RT_FLT  = 2.U(RT_SZ.W)
    val RT_IMM  = 3.U(RT_SZ.W)
    val RT_PC   = 4.U(RT_SZ.W)
    //
    val MEM_SZ  = 3
    val MEM_X   = BitPat.dontCare(MEM_SZ)
    val MEM_NON = 0.U(MEM_SZ.W)
    val MEM_B   = 1.U(MEM_SZ.W)
    val MEM_H   = 2.U(MEM_SZ.W)
    val MEM_W   = 3.U(MEM_SZ.W)
    //
    val FU_SZ   = 3
    val FU_X    = BitPat.dontCare(FU_SZ)
    val FU_NON  = 0.U(FU_SZ.W)
    val FU_ALU  = 1.U(FU_SZ.W)
    val FU_BJP  = 2.U(FU_SZ.W)
    val FU_MUL  = 3.U(FU_SZ.W)
    val FU_DIV  = 4.U(FU_SZ.W)
    val FU_LSU  = 5.U(FU_SZ.W)
    val FU_FPU  = 6.U(FU_SZ.W)
    val FU_MDU  = 7.U(FU_SZ.W)
    //
    val LONG_IMM_SZ = 20
    val IMM = BitPat.dontCare(LONG_IMM_SZ)
    //
    val uopBits = 6
    val uopX        = BitPat.dontCare(uopBits)
    val nonuop      = 0.U(uopBits.W)
    //  BJP
    val uopBNE      = 1.U(uopBits.W)
    val uopBEQ      = 2.U(uopBits.W)
    val uopBLT      = 3.U(uopBits.W)
    val uopBGE      = 4.U(uopBits.W)
    val uopJAL      = 5.U(uopBits.W)
    val uopJALR     = 6.U(uopBits.W)
    //  ALU
    val uopAUIPC    = 1.U(uopBits.W)
    val uopLUI      = 2.U(uopBits.W)
    val uopADD      = 3.U(uopBits.W)
    val uopSLT      = 4.U(uopBits.W)
    val uopXOR      = 5.U(uopBits.W)
    val uopOR       = 6.U(uopBits.W)
    val uopAND      = 7.U(uopBits.W)
    val uopSLL      = 8.U(uopBits.W)
    val uopSRL      = 9.U(uopBits.W)
    val uopSRA      = 10.U(uopBits.W)
    val uopSUB      = 11.U(uopBits.W)
    val uopFENCE    = 12.U(uopBits.W)
    val uopECALL    = 13.U(uopBits.W)
    val uopEBREAK   = 14.U(uopBits.W)
    val uopWFI      = 15.U(uopBits.W)
    val uopMRET     = 16.U(uopBits.W)
    val uopURET     = 17.U(uopBits.W)
    val uopSRET     = 18.U(uopBits.W)
    val uopCSRRW    = 19.U(uopBits.W)
    val uopCSRRS    = 20.U(uopBits.W)
    val uopCSRRC    = 21.U(uopBits.W)
    val uopVMA      = 22.U(uopBits.W)
    val uopDRET     = 23.U(uopBits.W)
    val uopEXC      = 24.U(uopBits.W)
    val uopSYNC     = 25.U(uopBits.W)
    val uopSYNF     = 26.U(uopBits.W)
    val uopFENCEI   = 27.U(uopBits.W)
    //  MDU
    val uopMUL      = 1.U(uopBits.W)
    val uopMULH     = 2.U(uopBits.W)
    val uopMULHSU   = 3.U(uopBits.W)
    val uopMULHU    = 4.U(uopBits.W)
    val uopDIV      = 5.U(uopBits.W)
    val uopDIVU     = 6.U(uopBits.W)
    val uopREM      = 7.U(uopBits.W)
    val uopREMU     = 8.U(uopBits.W)
    //  LSU
    val uopLOAD      = 3.U(uopBits.W)
    val uopSTORE     = 4.U(uopBits.W)
    val uopFLW       = 1.U(uopBits.W)
    val uopFSW       = 2.U(uopBits.W)
    val uopFLD       = 23.U(uopBits.W)
    val uopFSD       = 24.U(uopBits.W)
    val uopLRW       = 5.U(uopBits.W)
    val uopLRW_EXCL  = 6.U(uopBits.W)
    val uopSCW       = 7.U(uopBits.W)
    val uopSCW_EXCL  = 8.U(uopBits.W)
    val uopAMOSWAP_W = 9.U(uopBits.W)
    val uopAMOADD_W  = 10.U(uopBits.W)
    val uopAMOXOR_W  = 11.U(uopBits.W)
    val uopAMOAND_W  = 12.U(uopBits.W)
    val uopAMOOR_W   = 13.U(uopBits.W)
    val uopAMOMIN_W  = 14.U(uopBits.W)
    val uopAMOMAX_W  = 15.U(uopBits.W)
    //  FPU
    val uopFMADD_S  = 3.U(uopBits.W)
    val uopFMSUB_S  = 4.U(uopBits.W)
    val uopFNMSUB_S = 5.U(uopBits.W)
    val uopFNMADD_S = 6.U(uopBits.W)
    val uopFADD_S   = 7.U(uopBits.W)
    val uopFSUB_S   = 8.U(uopBits.W)
    val uopFMUL_S   = 9.U(uopBits.W)
    val uopFDIV_S   = 10.U(uopBits.W)
    val uopFSQRT_S  = 11.U(uopBits.W)
    val uopFSGNJ_S  = 12.U(uopBits.W)
    val uopFMIN_S   = 13.U(uopBits.W)
    val uopFMAX_S   = 14.U(uopBits.W)
    val uopFCVT_X_S = 15.U(uopBits.W)
    val uopFMV_S_X  = 16.U(uopBits.W)
    val uopFEQ_S    = 17.U(uopBits.W)
    val uopFLT_S    = 18.U(uopBits.W)
    val uopFLE_S    = 19.U(uopBits.W)
    val uopFCLASS_S = 20.U(uopBits.W)
    val uopFCVT_S_X = 21.U(uopBits.W)
    val uopFMV_X_S  = 22.U(uopBits.W)
    val uopFMADD_D  = 25.U(uopBits.W)
    val uopFMSUB_D  = 26.U(uopBits.W)
    val uopFNMSUB_D = 27.U(uopBits.W)
    val uopFNMADD_D = 28.U(uopBits.W)
    val uopFADD_D   = 29.U(uopBits.W)
    val uopFSUB_D   = 30.U(uopBits.W)
    val uopFMUL_D   = 31.U(uopBits.W)
    val uopFDIV_D   = 32.U(uopBits.W)
    val uopFSQRT_D  = 33.U(uopBits.W)
    val uopFSGNJ_D  = 34.U(uopBits.W)
    val uopFMIN_D   = 35.U(uopBits.W)
    val uopFMAX_D   = 36.U(uopBits.W)
    val uopFCVT_S_D = 37.U(uopBits.W)
    val uopFCVT_D_S = 38.U(uopBits.W)
    val uopFEQ_D    = 39.U(uopBits.W)
    val uopFLT_D    = 40.U(uopBits.W)
    val uopFLE_D    = 41.U(uopBits.W)
    val uopFCLASS_D = 42.U(uopBits.W)
    val uopFCVT_X_D = 43.U(uopBits.W)
    val uopFCVT_D_X = 44.U(uopBits.W)
}

object PRV {
    val PRV_SZ = 3
    val M   = 0.U(PRV_SZ.W)
    val S   = 2.U(PRV_SZ.W)
    val U   = 3.U(PRV_SZ.W)
    val LM  = 4.U(PRV_SZ.W)
    val LS  = 6.U(PRV_SZ.W)
    val LU  = 7.U(PRV_SZ.W)
}

class IssueSlotEntry(implicit p: Parameters) extends LumiaBundle {
    val valid           = Bool()
    val rob_id          = UInt(width = robIdBits)
    val st_val          = Bool()
    val stq_id          = UInt(width = stqIdBits)
    val pcq_val         = Bool()
    val pcq_id          = UInt(width = pcqIdBits)
    val paq_val         = Bool()
    val paq_id          = UInt(width = paqIdBits)
    val s2_bpu_pred     = new s2_bpu_pred
    val gc_tag          = UInt(width = gcTagSz)
    val exc_cause       = UInt(width = xLen)
    val uop             = new Uop
}

trait issue {
    def NullUop()(implicit p: Parameters): IssueSlotEntry = {
        val entry = Wire(new IssueSlotEntry)
        entry              := chisel3.DontCare
        entry.valid        := false.B
        entry.rob_id       := 0.U
        entry.st_val       := false.B
        entry.stq_id       := 0.U
        entry.pcq_val      := false.B
        entry.pcq_id       := 0.U
        entry.paq_val      := false.B
        entry.paq_id       := 0.U
        entry.s2_bpu_pred  := 0.U
        entry.uop          := 0.U
        entry
    }
}