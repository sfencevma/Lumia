package Lumia.exu

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.CSR
import freechips.rocketchip.tile._
import Lumia.cache._
import Lumia.common._
import Lumia.iq._

class BjpKillReq(implicit p: Parameters) extends LumiaBundle {
    val rob_id = UInt(width = robIdBits)
    val stq_id = UInt(width = stqIdBits)
    val pcq_val = Bool()
    val pcq_id = UInt(width = pcqIdBits)
    val paq_val = Bool()
    val paq_id = UInt(width = paqIdBits)
    val gc_tag = UInt(width = gcTagSz)
    val taddr  = UInt(width = vaddrBits)
}

class CSRResp(implicit p: Parameters) extends LumiaBundle {
    val addr = UInt(width = CSR.ADDRSZ)
    val wdata = UInt(width = xLen)
}

class FFlagsResp(implicit p: Parameters) extends LumiaBundle {
    val flags = Bits(width = FPConstants.FLAGS_SZ.W)
}

class ExecUnitId(implicit p: Parameters) extends LumiaBundle {
    val rob_id  = UInt(width = robIdBits)
    val stq_id  = UInt(width = stqIdBits)
    val pcq_val = Bool()
    val pcq_id  = UInt(width = pcqIdBits)
    val paq_val = Bool()
    val paq_id  = UInt(width = paqIdBits)
    val gc_tag  = UInt(width = gcTagSz)
}

class ExecUnitReq(dataWidth: Int)(implicit p: Parameters) extends LumiaBundle {
    val uop      = new Uop
    val cause    = UInt(width = xLen)
    val ip       = UInt(width = LONG_IMM_SZ)
    val rs1_data = UInt(width = dataWidth)
    val rs2_data = UInt(width = dataWidth)
    val rs3_data = UInt(width = dataWidth)
    val fcsr_rm  = UInt(width = FPConstants.RM_SZ)
    val ids      = new ExecUnitId
    val kill     = Bool()
}

class ExecUnitResp(dataWidth: Int)(implicit p: Parameters) extends LumiaBundle{
    val prd  = UInt(width = pregSz)
    val data = UInt(width = dataWidth)
}

class LSUExecIO(implicit p: Parameters) extends LumiaBundle {
    val enq = Vec(dispatchWidth, Valid(new StoreQueueReq).flip)
}


abstract class ExecUnit (
    val readIrf: Boolean = false,
    val writeIrf: Boolean = false,
    val readFrf: Boolean = false,
    val writeFrf: Boolean = false,
    val dataWidth: Int = 32,
    val hasAlu: Boolean = false,
    val hasMem: Boolean = false,
    val hasCSR: Boolean = false,
    val hasBjp: Boolean = false,
    val hasMdu: Boolean = false,
    val hasFdiv: Boolean = false,
    val hasFpu: Boolean = false,
    val hasFcsr: Boolean = false,
    val hasAddr: Boolean = false
)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new Bundle() {
        val fu_types = Valid(Bits(width = FU_SZ))
        val req = Flipped(Valid(new ExecUnitReq(dataWidth)))
        val iresp = if (writeIrf) Valid(new ExecUnitResp(dataWidth)) else null
        val fresp = if (writeFrf) Valid(new ExecUnitResp(dataWidth)) else null

        //  CSR
        val csr_req = if (hasCSR) UInt(INPUT, width = xLen) else null
        val csr_resp = if (hasCSR) Valid(new CSRResp) else null

        //  FPU
        val fcsr_rm = if (hasFcsr) Bits(INPUT, width = FPConstants.RM_SZ) else null
        val fflags = if (hasFcsr) Valid(new FFlagsResp) else null

        //  BJP
        val s2_bpu_pred = if (hasBjp) new s2_bpu_pred().asInput else null
        val bjp_kill = if (hasBjp) Valid(new BjpKillReq) else null
        val s2_pred_resp = if (hasBjp) Valid(new s2_pred_resp) else null
        val taddr = if (hasBjp) UInt(INPUT, width = vaddrBits) else null

        //  LSU
        val lsu_io = if (hasMem) Flipped(new LSUExecIO) else null

        //  Wakeup
        val wakeup = Valid(UInt(width = pregSz.W))

        //  Exec done
        val exc_done = Bool(OUTPUT)
        val exc_rob_id = UInt(OUTPUT, width = robIdBits)
        val cause = UInt(OUTPUT, width = xLen)
        val sfence = if (hasAlu) new SFenceReq().asOutput else null

        //
        val addr_hi = if(hasAddr) UInt(INPUT, width = 20) else null
        val addr_lo = if(hasAddr) UInt(INPUT, width = 12) else null
    })

    if (writeIrf) {
        io.fflags.valid := false.B
    }

    def hasFFlags = hasFpu || hasFdiv
}

/***
 * Execution stage
 */
class ExecutionStageIn(implicit p: Parameters) extends LumiaBundle {
    val ireq = Vec(issueWidth - 2, Valid(new ExecUnitReq(iregBits)).flip)
    val freq = Valid(new ExecUnitReq(fregBits).flip)

    //  LSU

}

class ExecutionStageOut(implicit p: Parameters) extends LumiaBundle {
    val fu_types = Vec(issueWidth, Valid(Bits(width = FU_SZ)))
}

class ExecutionStage(implicit p: Parameters) extends LumiaModule {
    val io = IO(new Bundle() {
        val in = new ExecutionStageIn
        val out = new ExecutionStageOut
    })
}



