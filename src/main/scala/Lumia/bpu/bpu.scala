package Lumia.bpu

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._

class s1_bpu_pred(implicit p: Parameters) extends LumiaBundle {
    val btb = new BTBResp
    val predictor = new PredictorResp
}

class s1_pred_resp(implicit p: Parameters) extends LumiaBundle {
    val btb      = new BTBUpdate
    val predictor = new PredictorUpdate
}

class BPUIO(implicit p: Parameters) extends LumiaBundle {
    val i_kill = new Bundle() {
        val trap_kill = Bool(INPUT)
        val bjp_kill = Bool(INPUT)
    }
    val req_pc = Valid(UInt(width = vaddrBits)).flip 
    val s1_pred_resp = Valid(new s1_pred_resp).flip 
    val s1_bpu_pred = Valid(new s1_bpu_pred)
    val s1_bpu_kill = Valid(UInt(width = vaddrBits))
}

class BPU(implicit p: Parameters) extends LumiaModule {
    val io = IO(new BPUIO)
    val kill = io.i_kill.trap_kill || io.i_kill.bjp_kill
    
    //****************************************
    //  Module body
    val btb = Module(new BTB).io 
    val predictor = Module(new Predictor).io
    
    //  BTB
    btb.req := io.req_pc
    btb.update.valid := io.s1_pred_resp.valid
    btb.update.bits := io.s1_pred_resp.bits.btb
    
    //  Predictor
    predictor.req := io.req_pc
    val res = Cat(io.s1_pred_resp.valid
            ,   io.s1_pred_resp.bits.btb.br_new
            ,   io.s1_pred_resp.bits.btb.tsucc
            ,   io.s1_pred_resp.bits.btb.alias_err)
    predictor.update.valid := io.s1_pred_resp.valid && (res === BitPat("b1000") || res === BitPat("b1010"))
    predictor.update.bits := io.s1_pred_resp.bits.predictor
    
    //  
    val btb_resp = RegNext(btb.resp)
    io.s1_bpu_pred.valid := RegNext(!kill & io.req_pc.valid)
    io.s1_bpu_pred.bits.btb := btb_resp
    io.s1_bpu_pred.bits.btb.index := RegNext(btb.index)
    io.s1_bpu_pred.bits.btb.addr := RegNext(io.req_pc.bits)
    io.s1_bpu_pred.bits.btb.taken := !btb_resp.inst_type | predictor.resp.table_taken
    io.s1_bpu_pred.bits.predictor := predictor.resp
    
    //  Flush signal
    io.s1_bpu_kill := (btb.resp.hit
      & (io.req_pc.bits(vaddrBits - 1, 4) === btb.resp.taddr(vaddrBits - 1, 4))
      & (io.req_pc.bits(3, 0) <= btb.resp.offset)
      & (!btb.resp.inst_type | (btb.resp.inst_type & !predictor.resp.table_taken)))
    io.s1_bpu_kill.bits := btb.resp.taddr
}