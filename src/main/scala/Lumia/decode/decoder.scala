package Lumia.decode


import Chisel._
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util.uintToBitPat
import freechips.rocketchip.config.Parameters
import Lumia.common._


abstract trait DecoderConstants extends freechips.rocketchip.rocket.constants.ScalarOpConstants
  with  freechips.rocketchip.rocket.constants.MemoryOpConstants {
    def decoder_default: List[BitPat] = List (
        Y,      //  ilgl
        LEN_X,  //  len
        RT_X,   //  rs1 reg type
        RT_X,   //  rs2 reg type
        RT_X,   //  rs3 reg type
        RT_X,   //  rd reg type
        IF_X,   //  Imm sel
        S,      //  signed op
        FU_X,   //  Functions unit
        N,      //  AMO
        uopX,   //  micro-op
        MEM_X   //  mem size
    )

    val table: Array[(BitPat, List[BitPat])]
}


object IDecode extends DecoderConstants {
    val table: Array[(BitPat, List[BitPat])] = Array (
        BNE     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_B, S, FU_BJP, N,  uopBNE,   MEM_W),
        BEQ     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_B, S, FU_BJP, N,  uopBEQ,   MEM_W),
        BLT     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_B, S, FU_BJP, N,  uopBLT,   MEM_W),
        BLTU    -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_B, U, FU_BJP, N,  uopBLT,   MEM_W),
        BGE     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_B, S, FU_BJP, N,  uopBGE,   MEM_W),
        BGEU    -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_B, U, FU_BJP, N,  uopBGE,   MEM_W),

        JAL     -> List(N, LEN_32, RT_PC,  RT_IMM, RT_NON, RT_REG, IF_J, S, FU_BJP, N,  uopJAL,   MEM_W),
        JALR    -> List(N, LEN_32, RT_REG, RT_IMM, RT_NON, RT_REG, IF_I, S, FU_BJP, N,  uopJALR,  MEM_W),
        AUIPC   -> List(N, LEN_32, RT_PC,  RT_IMM, RT_NON, RT_REG, IF_U, S, FU_BJP, N,  uopAUIPC, MEM_W),

        LB      -> List(N, LEN_32, RT_REG, RT_NON, RT_NON, RT_REG, IF_I, S, FU_LSU, N,  uopLOAD,  MEM_B),
        LH      -> List(N, LEN_32, RT_REG, RT_NON, RT_NON, RT_REG, IF_I, S, FU_LSU, N,  uopLOAD,  MEM_H),
        LW      -> List(N, LEN_32, RT_REG, RT_NON, RT_NON, RT_REG, IF_I, S, FU_LSU, N,  uopLOAD,  MEM_W),
        LBU     -> List(N, LEN_32, RT_REG, RT_NON, RT_NON, RT_REG, IF_I, U, FU_LSU, N,  uopLOAD,  MEM_B),
        LHU     -> List(N, LEN_32, RT_REG, RT_NON, RT_NON, RT_REG, IF_I, U, FU_LSU, N,  uopLOAD,  MEM_H),
        SB      -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_S, S, FU_LSU, N,  uopSTORE, MEM_B),
        SH      -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_S, S, FU_LSU, N,  uopSTORE, MEM_H),
        SW      -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_S, S, FU_LSU, N,  uopSTORE, MEM_W),

        LUI     -> List(N, LEN_32, RT_IMM, RT_NON, RT_NON, RT_REG, IF_U, S, FU_ALU, N,  uopLUI,   MEM_W),
        ADDI    -> List(N, LEN_32, RT_REG, RT_IMM, RT_NON, RT_REG, IF_I, S, FU_ALU, N,  uopADD,   MEM_W),
        SLTI    -> List(N, LEN_32, RT_REG, RT_IMM, RT_NON, RT_REG, IF_I, S, FU_ALU, N,  uopSLT,   MEM_W),
        SLTIU   -> List(N, LEN_32, RT_REG, RT_IMM, RT_NON, RT_REG, IF_I, U, FU_ALU, N,  uopSLT,   MEM_W),
        XORI    -> List(N, LEN_32, RT_REG, RT_IMM, RT_NON, RT_REG, IF_I, S, FU_ALU, N,  uopXOR,   MEM_W),
        ORI     -> List(N, LEN_32, RT_REG, RT_IMM, RT_NON, RT_REG, IF_I, S, FU_ALU, N,  uopOR,    MEM_W),
        ANDI    -> List(N, LEN_32, RT_REG, RT_IMM, RT_NON, RT_REG, IF_I, S, FU_ALU, N,  uopAND,   MEM_W),
        SLLI    -> List(N, LEN_32, RT_REG, RT_IMM, RT_NON, RT_REG, IF_I, S, FU_ALU, N,  uopSLL,   MEM_W),
        SRLI    -> List(N, LEN_32, RT_REG, RT_IMM, RT_NON, RT_REG, IF_I, S, FU_ALU, N,  uopSRL,   MEM_W),
        SRAI    -> List(N, LEN_32, RT_REG, RT_IMM, RT_NON, RT_REG, IF_I, S, FU_ALU, N,  uopSRA,   MEM_W),
        ADD     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_ALU, N,  uopADD,   MEM_W),
        SUB     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_ALU, N,  uopSUB,   MEM_W),
        SLL     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_ALU, N,  uopSLL,   MEM_W),
        SLT     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_ALU, N,  uopSLT,   MEM_W),
        SLTU    -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, U, FU_ALU, N,  uopSLT,   MEM_W),
        XOR     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_ALU, N,  uopXOR,   MEM_W),
        SRL     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_ALU, N,  uopSRL,   MEM_W),
        SRA     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_ALU, N,  uopSRA,   MEM_W),
        OR      -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_ALU, N,  uopOR,    MEM_W),
        AND     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_ALU, N,  uopAND,   MEM_W),

        FENCE   -> List(N, LEN_32, RT_REG, RT_NON, RT_NON, RT_REG, IF_I, S, FU_ALU, N,  uopFENCE, MEM_W),

        ECALL   -> List(N, LEN_32, RT_NON, RT_NON, RT_NON, RT_NON, IF_I, S, FU_ALU, N,  uopECALL, MEM_W),
        EBREAK  -> List(N, LEN_32, RT_NON, RT_NON, RT_NON, RT_NON, IF_I, S, FU_ALU, N,  uopEBREAK, MEM_W),
        WFI     -> List(N, LEN_32, RT_NON, RT_NON, RT_NON, RT_NON, IF_X, S, FU_ALU, N,  uopWFI,   MEM_W),
        MRET    -> List(N, LEN_32, RT_NON, RT_NON, RT_NON, RT_NON, IF_X, S, FU_ALU, N,  uopMRET,  MEM_W),
        URET    -> List(N, LEN_32, RT_NON, RT_NON, RT_NON, RT_NON, IF_X, S, FU_ALU, N,  uopURET,  MEM_W),

        CSRRW   -> List(N, LEN_32, RT_REG, RT_NON, RT_NON, RT_REG, IF_I, U, FU_ALU, N,  uopCSRRW, MEM_W),
        CSRRS   -> List(N, LEN_32, RT_REG, RT_NON, RT_NON, RT_REG, IF_I, U, FU_ALU, N,  uopCSRRS, MEM_W),
        CSRRC   -> List(N, LEN_32, RT_REG, RT_NON, RT_NON, RT_REG, IF_I, U, FU_ALU, N,  uopCSRRC, MEM_W),
        CSRRWI  -> List(N, LEN_32, RT_REG, RT_NON, RT_IMM, RT_REG, IF_I, U, FU_ALU, N,  uopCSRRW, MEM_W),
        CSRRSI  -> List(N, LEN_32, RT_REG, RT_NON, RT_IMM, RT_REG, IF_I, U, FU_ALU, N,  uopCSRRS, MEM_W),
        CSRRCI  -> List(N, LEN_32, RT_REG, RT_NON, RT_IMM, RT_REG, IF_I, U, FU_ALU, N,  uopCSRRC, MEM_W)
    )
}

object FenceIDecode extends DecoderConstants {
    val table: Array[(BitPat, List[BitPat])] = Array (
        FENCE_I     -> List(N, LEN_32, RT_REG, RT_IMM, RT_NON, RT_REG, IF_I, S, FU_ALU, N,  uopFENCEI, MEM_W)
    )
}

object SDecode extends DecoderConstants {
    val table: Array[(BitPat, List[BitPat])] = Array (
        SRET        -> List(N, LEN_32, RT_NON, RT_NON, RT_NON, RT_NON, IF_X, S, FU_ALU, N,  uopSRET, MEM_W),
        SFENCE_VMA  -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_NON, IF_X, S, FU_ALU, N,  uopVMA,  MEM_W)
    )
}

object MDecode extends DecoderConstants {
    val table: Array[(BitPat, List[BitPat])] = Array (
        MUL     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_MUL, N,  uopMUL,    MEM_B),
        MULH    -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_MUL, N,  uopMULH,   MEM_H),
        MULHSU  -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_MUL, N,  uopMULHSU, MEM_H),
        MULHU   -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, U, FU_MUL, N,  uopMULHU,  MEM_H),
        DIV     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_DIV, N,  uopDIV,    MEM_W),
        DIVU    -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, U, FU_DIV, N,  uopDIVU,   MEM_W),
        REM     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_DIV, N,  uopREM,    MEM_W),
        REMU    -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, U, FU_DIV, N,  uopREMU,   MEM_W)
    )
}

object DebugDecode extends DecoderConstants {
    val table: Array[(BitPat, List[BitPat])] = Array (
        DRET    -> List(N, LEN_32, RT_NON, RT_NON, RT_NON, RT_NON, IF_X, S, FU_ALU, N,  uopDRET, MEM_W)
    )
}

object ADecode extends DecoderConstants {
    val table: Array[(BitPat, List[BitPat])] = Array (
        LR_W        -> List(N, LEN_32, RT_REG, RT_NON, RT_NON, RT_REG, IF_X, S, FU_LSU, Y,  uopLRW,         MEM_W),
        SC_W        -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_LSU, Y,  uopSCW,         MEM_W),
        AMOSWAP_W   -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_LSU, Y,  uopAMOSWAP_W,   MEM_W),
        AMOADD_W    -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_LSU, Y,  uopAMOADD_W,    MEM_W),
        AMOXOR_W    -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_LSU, Y,  uopAMOXOR_W,    MEM_W),
        AMOAND_W    -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_LSU, Y,  uopAMOAND_W,    MEM_W),
        AMOOR_W     -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_LSU, Y,  uopAMOOR_W,     MEM_W),
        AMOMIN_W    -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_LSU, Y,  uopAMOMIN_W,    MEM_W),
        AMOMAX_W    -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, S, FU_LSU, Y,  uopAMOMAX_W,    MEM_W),
        AMOMINU_W   -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, U, FU_LSU, Y,  uopAMOMIN_W,    MEM_W),
        AMOMAXU_W   -> List(N, LEN_32, RT_REG, RT_REG, RT_NON, RT_REG, IF_X, U, FU_LSU, Y,  uopAMOMAX_W,    MEM_W)
    )
}

object FDecode extends DecoderConstants {
    val table: Array[(BitPat, List[BitPat])] = Array (
        FLW         -> List(N, LEN_32,  RT_FLT, RT_IMM, RT_NON, RT_FLT, IF_X, S, FU_LSU, N,  uopFLW,      MEM_B),
        FSW         -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_LSU, N,  uopFSW,      MEM_B),
        FMADD_S     -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_FLT, RT_FLT, IF_X, S, FU_FPU, N,  uopFMADD_S,  MEM_B),
        FMSUB_S     -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_FLT, RT_FLT, IF_X, S, FU_FPU, N,  uopFMSUB_S,  MEM_B),
        FNMSUB_S    -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_FLT, RT_FLT, IF_X, S, FU_FPU, N,  uopFNMSUB_S, MEM_B),
        FNMADD_S    -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_FLT, RT_FLT, IF_X, S, FU_FPU, N,  uopFNMADD_S, MEM_B),
        FADD_S      -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFADD_S,   MEM_B),
        FSUB_S      -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFSUB_S,   MEM_B),
        FMUL_S      -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFMUL_S,   MEM_B),
        FDIV_S      -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFDIV_S,   MEM_B),
        FSQRT_S     -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFSQRT_S,  MEM_B),
        FSGNJ_S     -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFSGNJ_S,  MEM_B),
        FSGNJX_S    -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFSGNJ_S,  MEM_B),
        FSGNJN_S    -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFSGNJ_S,  MEM_B),
        FMIN_S      -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFMIN_S,   MEM_B),
        FMAX_S      -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFMAX_S,   MEM_B),
        FCVT_W_S    -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFCVT_X_S, MEM_B),
        FCVT_WU_S   -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, U, FU_FPU, N,  uopFCVT_X_S, MEM_B),
        FMV_X_W     -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, U, FU_FPU, N,  uopFMV_S_X,  MEM_B),
        FEQ_S       -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFEQ_S,    MEM_B),
        FLT_S       -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFLT_S,    MEM_B),
        FLE_S       -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFLE_S,    MEM_B),
        FCLASS_S    -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFCLASS_S, MEM_B),
        FCVT_S_W    -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFCVT_S_X, MEM_B),
        FCVT_S_WU   -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, U, FU_FPU, N,  uopFCVT_S_X, MEM_B),
        FMV_W_X     -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFMV_X_S,  MEM_B)
    )
}

object DDecode extends DecoderConstants {
    val table: Array[(BitPat, List[BitPat])] = Array (
        FLD         -> List(N, LEN_32,  RT_FLT, RT_IMM, RT_NON, RT_FLT, IF_X, S, FU_LSU, N,  uopFLD,      MEM_H),
        FSD         -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_LSU, N,  uopFSD,      MEM_H),
        FMADD_D     -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_FLT, RT_FLT, IF_X, S, FU_FPU, N,  uopFMADD_D,  MEM_H),
        FMSUB_D     -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_FLT, RT_FLT, IF_X, S, FU_FPU, N,  uopFMSUB_D,  MEM_H),
        FNMSUB_D    -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_FLT, RT_FLT, IF_X, S, FU_FPU, N,  uopFNMSUB_D, MEM_H),
        FNMADD_D    -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_FLT, RT_FLT, IF_X, S, FU_FPU, N,  uopFNMADD_D, MEM_H),
        FADD_D      -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFADD_D,   MEM_H),
        FSUB_D      -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFSUB_D,   MEM_H),
        FMUL_D      -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFMUL_D,   MEM_H),
        FDIV_D      -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFDIV_D,   MEM_H),
        FSQRT_D     -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFSQRT_D,  MEM_H),
        FSGNJ_D     -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFSGNJ_D,  MEM_H),
        FSGNJX_D    -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFSGNJ_D,  MEM_H),
        FSGNJN_D    -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFSGNJ_D,  MEM_H),
        FMIN_D      -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFMIN_D,   MEM_H),
        FMAX_D      -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFMAX_D,   MEM_H),
        FCVT_S_D    -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFCVT_S_D, MEM_H),
        FCVT_D_S    -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFCVT_D_S, MEM_B),
        FEQ_D       -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFEQ_D,    MEM_H),
        FLT_D       -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFLT_D,    MEM_H),
        FLE_D       -> List(N, LEN_32,  RT_FLT, RT_FLT, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFLE_D,    MEM_H),
        FCLASS_D    -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFCLASS_D, MEM_H),
        FCVT_W_D    -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFCVT_X_D, MEM_H),
        FCVT_WU_D   -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, U, FU_FPU, N,  uopFCVT_X_D, MEM_H),
        FCVT_D_W    -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, S, FU_FPU, N,  uopFCVT_D_X, MEM_B),
        FCVT_D_WU   -> List(N, LEN_32,  RT_FLT, RT_NON, RT_NON, RT_FLT, IF_X, U, FU_FPU, N,  uopFCVT_D_X, MEM_B)
    )
}

class ExpandedInstruction(implicit p: Parameters) extends LumiaBundle {
    val bits    = UInt(width = instBits)
    val rd      = UInt(width = lregSz)
    val rs1     = UInt(width = lregSz)
    val rs2     = UInt(width = lregSz)
    val rs3     = UInt(width = lregSz)
}

class RVCDecoder(x: UInt, xLen: Int)(implicit p: Parameters) {
    def inst(bits: UInt, rd: UInt = x(11,7), rs1: UInt = x(19,15), rs2: UInt = x(24,20), rs3: UInt = x(31,27)) = {
        val res = Wire(new ExpandedInstruction)
        res.bits := bits
        res.rd := rd
        res.rs1 := rs1
        res.rs2 := rs2
        res.rs3 := rs3
        res
    }

    def passthrough = inst(x)

    def rs1p = Cat(1.U(2.W), x(9,7))
    def rs2p = Cat(1.U(2.W), x(4,2))
    def rs2 = x(6,2)
    def rd = x(11,7)
    def addi4spnImm = Cat(x(10,7), x(12,11), x(5), x(6), 0.U(2.W))
    def lwImm = Cat(x(5), x(12,10), x(6), 0.U(2.W))
    def ldImm = Cat(x(6,5), x(12,10), 0.U(3.W))
    def lwspImm = Cat(x(3,2), x(12), x(6,4), 0.U(2.W))
    def ldspImm = Cat(x(4,2), x(12), x(6,5), 0.U(3.W))
    def swspImm = Cat(x(8,7), x(12,9), 0.U(2.W))
    def sdspImm = Cat(x(9,7), x(12,10), 0.U(3.W))
    def luiImm = Cat(Fill(15, x(12)), x(6,2), 0.U(12.W))
    def addi16spImm = Cat(Fill(3, x(12)), x(4,3), x(5), x(2), x(6), 0.U(4.W))
    def addiImm = Cat(Fill(7, x(12)), x(6,2))
    def jImm = Cat(Fill(10, x(12)), x(8), x(10,9), x(6), x(7), x(2), x(11), x(5,3), 0.U(1.W))
    def bImm = Cat(Fill(5, x(12)), x(6,5), x(2), x(11,10), x(4,3), 0.U(1.W))
    def shamt = Cat(x(12), x(6,2))
    def x0 = 0.U(5.W)
    def ra = 1.U(5.W)
    def sp = 2.U(5.W)

    def q0 = {
        def addi4spn = {
            val opc = Mux(x(12,5).orR, 0x13.U(7.W), 0x1F.U(7.W))
            inst(Cat(addi4spnImm, sp, 0.U(3.W), rs2p, opc), rs2p, sp, rs2p)
        }
        def ld = inst(Cat(ldImm, rs1p, 3.U(3.W), rs2p, 0x03.U(7.W)), rs2p, rs1p, rs2p)
        def lw = inst(Cat(lwImm, rs1p, 2.U(3.W), rs2p, 0x03.U(7.W)), rs2p, rs1p, rs2p)
        def fld = inst(Cat(ldImm, rs1p, 3.U(3.W), rs2p, 0x07.U(7.W)), rs2p, rs1p, rs2p)
        def flw = {
            if (xLen == 32) inst(Cat(lwImm, rs1p, 2.U(3.W), rs2p, 0x07.U(7.W)), rs2p, rs1p, rs2p)
            else ld
        }
        def unimp = inst(Cat(lwImm >> 5, rs2p, rs1p, 2.U(3.W), lwImm(4,0), 0x3F.U(7.W)), rs2p, rs1p, rs2p)
        def sd = inst(Cat(ldImm >> 5, rs2p, rs1p, 3.U(3.W), ldImm(4,0), 0x23.U(7.W)), rs2p, rs1p, rs2p)
        def sw = inst(Cat(lwImm >> 5, rs2p, rs1p, 2.U(3.W), lwImm(4,0), 0x23.U(7.W)), rs2p, rs1p, rs2p)
        def fsd = inst(Cat(ldImm >> 5, rs2p, rs1p, 3.U(3.W), ldImm(4,0), 0x27.U(7.W)), rs2p, rs1p, rs2p)
        def fsw = {
            if (xLen == 32) inst(Cat(lwImm >> 5, rs2p, rs1p, 2.U(3.W), lwImm(4,0), 0x27.U(7.W)), rs2p, rs1p, rs2p)
            else sd
        }
        Seq(addi4spn, fld, lw, flw, unimp, fsd, sw, fsw)
    }

    def q1 = {
        def addi = inst(Cat(addiImm, rd, 0.U(3.W), rd, 0x13.U(7.W)), rd, rd, rs2p)
        def addiw = {
            val opc = Mux(rd.orR, 0x1B.U(7.W), 0x1F.U(7.W))
            inst(Cat(addiImm, rd, 0.U(3.W), rd, opc), rd, rd, rs2p)
        }
        def jal = {
            if (xLen == 32) inst(Cat(jImm(20), jImm(10,1), jImm(11), jImm(19,12), ra, 0x6F.U(7.W)), ra, rd, rs2p)
            else addiw
        }
        def li = inst(Cat(addiImm, x0, 0.U(3.W), rd, 0x13.U(7.W)), rd, x0, rs2p)
        def addi16sp = {
            val opc = Mux(addiImm.orR, 0x13.U(7.W), 0x1F.U(7.W))
            inst(Cat(addi16spImm, rd, 0.U(3.W), rd, opc), rd, rd, rs2p)
        }
        def lui = {
            val opc = Mux(addiImm.orR, 0x37.U(7.W), 0x3F.U(7.W))
            val me = inst(Cat(luiImm(31,12), rd, opc), rd, rd, rs2p)
            Mux(rd === x0 || rd === sp, addi16sp, me)
        }
        def j = inst(Cat(jImm(20), jImm(10,1), jImm(11), jImm(19,12), x0, 0x6F.U(7.W)), x0, rs1p, rs2p)
        def beqz = inst(Cat(bImm(12), bImm(10,5), x0, rs1p, 0.U(3.W), bImm(4,1), bImm(11), 0x63.U(7.W)), rs1p, rs1p, x0)
        def bnez = inst(Cat(bImm(12), bImm(10,5), x0, rs1p, 1.U(3.W), bImm(4,1), bImm(11), 0x63.U(7.W)), x0, rs1p, x0)
        def arith = {
            def srli = Cat(shamt, rs1p, 5.U(3.W), rs1p, 0x13.U(7.W))
            def srai = srli | (1 << 30).U
            def andi = Cat(addiImm, rs1p, 7.U(3.W), rs1p, 0x13.U(7.W))
            def rtype = {
                val funct = Vec(Seq(0.U, 4.U, 6.U, 7.U, 0.U, 0.U, 2.U, 3.U))(Cat(x(12), x(6,5)))
                val sub = Mux(x(6,5) === 0.U, (1 << 30).U, 0.U)
                val opc = Mux(x(12), 0x3B.U(7.W), 0x33.U(7.W))
                Cat(rs2p, rs1p, funct, rs1p, opc) | sub
            }
            inst(Vec(Seq(srli, srai, andi, rtype))(x(11,10)), rs1p, rs1p, rs2p)
        }
        Seq(addi, jal, li, lui, arith, j, beqz, bnez)
    }

    def q2 = {
        val load_opc = Mux(rd.orR, 0x03.U(7.W), 0x1F.U(7.W))
        def slli = inst(Cat(shamt, rd, 1.U(3.W), rd, 0x13.U(7.W)), rd, rd, rs2)
        def ldsp = inst(Cat(ldspImm, sp, 3.U(3.W), rd, load_opc), rd, sp, rs2)
        def lwsp = inst(Cat(lwspImm, sp, 2.U(3.W), rd, load_opc), rd, sp, rs2)
        def fldsp = inst(Cat(ldspImm, sp, 3.U(3.W), rd, 0x07.U(7.W)), rd, sp, rs2)
        def flwsp = {
            if (xLen == 32) inst(Cat(lwspImm, sp, 2.U(3.W), rd, 0x07.U(7.W)), rd, sp, rs2)
            else ldsp
        }
        def sdsp = inst(Cat(sdspImm >> 5, rs2, sp, 3.U(3.W), sdspImm(4,0), 0x23.U(7.W)), rd, sp, rs2)
        def swsp = inst(Cat(swspImm >> 5, rs2, sp, 2.U(3.W), swspImm(4,0), 0x23.U(7.W)), rd, sp, rs2)
        def fsdsp = inst(Cat(sdspImm >> 5, rs2, sp, 3.U(3.W), sdspImm(4,0), 0x27.U(7.W)), rd, sp, rs2)
        def fswsp = {
            if (xLen == 32) inst(Cat(swspImm >> 5, rs2, sp, 2.U(3.W), swspImm(4,0), 0x27.U(7.W)), rd, sp, rs2)
            else sdsp
        }
        def jalr = {
            val mv = inst(Cat(rs2, x0, 0.U(3.W), rd, 0x33.U(7.W)), rd, x0, rs2)
            val add = inst(Cat(rs2, rd, 0.U(3.W), rd, 0x33.U(7.W)), rd, rd, rs2)
            val jr = Cat(rs2, rd, 0.U(3.W), x0, 0x67.U(7.W))
            val reserved = Cat(jr >> 7, 0x1F.U(7.W))
            val jr_reserved = inst(Mux(rd.orR, jr, reserved), x0, rd, rs2)
            val jr_mv = Mux(rs2.orR, mv, jr_reserved)
            val jalr = Cat(rs2, rd, 0.U(3.W), ra, 0x67.U(7.W))
            val ebreak = Cat(jr >> 7, 0x73.U(7.W)) | (1 << 20).U
            val jalr_ebreak = inst(Mux(rd.orR, jalr, ebreak), ra, rd, rs2)
            val jalr_add = Mux(rs2.orR, add, jalr_ebreak)
            Mux(x(12), jalr_add, jr_mv)
        }
        Seq(slli, fldsp, lwsp, flwsp, jalr, fsdsp, swsp, fswsp)
    }

    def q3 = Seq.fill(8)(passthrough)

    def decode = {
        val s = q0 ++ q1 ++ q2 ++ q3
        Vec(s)(Cat(x(1,0), x(15,13)))
    }
}

class DecoderSigs(implicit p: Parameters) extends LumiaBundle {
    val ilgl        = Bool()
    val len         = Bool()
    val lrs1_type   = UInt(width = RT_SZ)
    val lrs2_type   = UInt(width = RT_SZ)
    val lrs3_type   = UInt(width = RT_SZ)
    val lrd_type    = UInt(width = RT_SZ)
    val sign_op     = Bool()
    val imm_sel     = UInt(width = IF_SZ)
    val fu          = UInt(width = FU_SZ)
    val amo         = Bool()
    val uop         = UInt(width = uopBits)
    val mem_size    = UInt(width = MEM_SZ)

    def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
        val decoder = DecodeLogic(inst, IDecode.decoder_default, table)
        val sigs = Seq(ilgl, len, lrs1_type, lrs2_type, lrs3_type, lrd_type, imm_sel, sign_op, fu, amo, uop, mem_size)
        sigs zip decoder map {case (f, s) => f := s}
        this
    }
}

class DecoderIO(implicit p: Parameters) extends LumiaBundle {
    val inst = UInt(INPUT, width = instBits)
    val ilgl = Bool(OUTPUT)
    val uop = new Uop().asOutput
}

class Decoder(implicit p: Parameters) extends LumiaModule {
    val io = IO(new DecoderIO)

    def isRVC(inst: UInt) = inst(1, 0) =/= 3.U

    val rvc_expander = new RVCDecoder(io.inst, instBits).decode
    val inst = if (usingCompressed) {
        Mux(isRVC(io.inst), (new RVCDecoder(io.inst, instBits).decode).bits, io.inst)
    } else {
        io.inst
    }
    //
    val decode_table = IDecode.table ++ FenceIDecode.table ++ SDecode.table ++ MDecode.table ++ ADecode.table ++ DebugDecode.table ++ FDecode.table ++ DDecode.table
    val dsigs = Wire(new DecoderSigs).decode(inst, decode_table)

    val di24_20 = Mux(dsigs.imm_sel === IF_B || dsigs.imm_sel === IF_S, inst(11, 7), inst(24, 20))
    val ip = Cat(inst(31, 25), di24_20, inst(19, 12))

    val amo_aq = io.inst(26)
    val amo_rl = io.inst(25)
    val amo_excl = amo_aq | amo_rl

    val amo_lr_excl = dsigs.amo && dsigs.uop === uopLRW && amo_excl
    val amo_sc_excl = dsigs.amo && dsigs.uop === uopSCW && amo_excl

    val uop = Mux(amo_lr_excl, uopLRW_EXCL, Mux(amo_sc_excl, uopSCW_EXCL, dsigs.uop))

    io.ilgl := dsigs.ilgl
    io.uop.len := dsigs.len
    io.uop.rs1_type := dsigs.lrs1_type
    io.uop.rs1 := Cat(Fill(pregSz - lregSz, 0.U), inst(19, 15))
    io.uop.rs2_type := dsigs.lrs2_type
    io.uop.rs2 := Cat(Fill(pregSz - lregSz, 0.U), inst(24, 20))
    io.uop.rs3_type := dsigs.lrs3_type
    io.uop.rs3 := Cat(Fill(pregSz - lregSz, 0.U), inst(31, 27))
    io.uop.rd_type := dsigs.lrd_type
    io.uop.rd := Cat(Fill(pregSz - lregSz, 0.U), inst(11, 7))
    io.uop.imm_sel := dsigs.imm_sel
    io.uop.imm_package := ip
    io.uop.sign_op := dsigs.sign_op
    io.uop.fu := dsigs.fu
    io.uop.uop := uop
    io.uop.mem_size := dsigs.mem_size
}