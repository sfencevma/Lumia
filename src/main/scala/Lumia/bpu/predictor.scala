package Lumia.bpu

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import scala.math.min

class WeightTableUpdate(entryBits: Int)(implicit p: Parameters) extends LumiaBundle {
    val index = UInt(width = log2Ceil(numWeightTableEntries))
    val weight = SInt(width = entryBits)
}

class WeightTableIO(entryBits: Int)(implicit p: Parameters) extends LumiaBundle {
    val req    = Valid(UInt(vaddrBits.W)).flip
    val i_ghist  = UInt(INPUT, globalHistoryLength.W)
    val update = Valid(new WeightTableUpdate(entryBits)).flip
    val o_index  = UInt(OUTPUT, width = log2Ceil(numWeightTableEntries))
    val o_entry  = SInt(OUTPUT, width = entryBits)
}

class WeightTable(entryBits: Int, histLength: Int)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new WeightTableIO(entryBits))
    def preHash(pc: UInt) = (pc >> fetchBytes).asUInt()
    def compute_folded_hist(hist: UInt, len: Int) = {
        val nChunks = (histLength + len - 1) / len
        val hist_chunks = (0 until nChunks) map {
            i => hist(min((i + 1) * len, histLength) - 1, i * len)
        }
        hist_chunks.reduce(_^_)
    }
    def compute_idx_hash(unhashed_id: UInt, hist: UInt) = {
        val idx_history = compute_folded_hist(hist, log2Ceil(numWeightTableEntries))
        (unhashed_id ^ idx_history)(log2Ceil(numWeightTableEntries) - 1, 0)
    }

    //********************************************
    //  Module body
    val table = chisel3.SyncReadMem(numWeightTableEntries, SInt(entryBits.W))
    val ridx = compute_idx_hash(preHash(io.req.bits), io.i_ghist)
    io.o_index := RegNext(ridx)
    io.o_entry := table.read(ridx, io.req.valid)
    when (io.update.valid) {
        table.write(io.update.bits.index, io.update.bits.weight)
    }
}

case class PredictorParams (
    tableInfo: Seq[Tuple2[Int, Int]] = Seq(  //entryWidth      histLength
                                                (3,                   8),
                                                (5,                  16),
                                                (4,                  32),
                                                (5,                  64))
)

class PredictorInfo(implicit p: Parameters) extends LumiaBundle {
    val table_base_id   = UInt(width = log2Ceil(numWeightTableEntries))
    val table_wa_id     = UInt(width = log2Ceil(numWeightTableEntries))
    val table_wb_id     = UInt(width = log2Ceil(numWeightTableEntries))
    val table_wc_id     = UInt(width = log2Ceil(numWeightTableEntries))
    val table_base_w    = SInt(width = 3)
    val table_wa_w      = SInt(width = 5)
    val table_wb_w      = SInt(width = 4)
    val table_wc_w      = SInt(width = 5)
}

class PredictorUpdate(implicit p: Parameters) extends PredictorInfo {
    val taken  = Bool()
}

class PredictorResp(implicit p: Parameters) extends PredictorInfo {
    val table_taken  = Bool()
}

class Predictor(params: PredictorParams = PredictorParams())(implicit p: Parameters) extends LumiaModule {
    private val numTables = params.tableInfo.size
    val io = IO(new Bundle() {
        val req = Valid(UInt(width = vaddrBits)).flip
        val update = Valid(new PredictorUpdate).flip
        val resp = new PredictorResp().asOutput
    })
    def INT_MAX(w: SInt) = Cat(0.U(1.W), Fill(w.getWidth - 1, 1.U)).asSInt()
    def INT_MIN(w: SInt) = Cat(1.U(1.W), Fill(w.getWidth - 1, 0.U)).asSInt()
    def inc_ctr(ctr: SInt, taken: Bool, step: Int) = {
        Mux(taken, Mux(ctr === INT_MAX(ctr), ctr, ctr + step.S)
            ,   Mux(ctr === INT_MIN(ctr), ctr, ctr - step.S))
    }
    val ghist = RegInit(0.U(globalHistoryLength.W))
    val tables = params.tableInfo map {
        case (e, l) => {
            val t = Module(new WeightTable(e, l)).io
            t.req := io.req
            t.i_ghist := ghist
            t
        }
    }
    val idx = tables.map(_.o_index)
    val entries = tables.map(_.o_entry)
    val sum_weight = (entries(0) << 2).asSInt() + entries(1).asSInt() + (entries(2) << 1).asSInt() + entries(3).asSInt()

    io.resp.table_taken      := sum_weight(sum_weight.getWidth - 1)
    io.resp.table_base_id    := idx(0)
    io.resp.table_base_w     := entries(0)
    io.resp.table_wa_id      := idx(1)
    io.resp.table_wa_w       := entries(1)
    io.resp.table_wb_id      := idx(2)
    io.resp.table_wb_w       := entries(2)
    io.resp.table_wc_id      := idx(3)
    io.resp.table_wc_w       := entries(3)

    //  Updating
    val update  = io.update
    val taken   = update.bits.taken
    val base_id = update.bits.table_base_id
    val base_w  = update.bits.table_base_w
    val wa_id   = update.bits.table_wa_id
    val wa_w    = update.bits.table_wa_w
    val wb_id   = update.bits.table_wb_id
    val wb_w    = update.bits.table_wb_w
    val wc_id   = update.bits.table_wc_id
    val wc_w    = update.bits.table_wc_w
    val update_widx = params.tableInfo map {
        case (e, l) => {
            Wire(UInt(width = log2Ceil(numWeightTableEntries)))
        }
    }

    val update_wdata = params.tableInfo map {
        case (e, l) => {
            Wire(SInt(e.W))
        }
    }
    //  Update base table
    val base_w_inc = base_w < 3.S & taken
    val base_w_dec = base_w > -3.S & !taken
    update_widx(0) := base_id
    update_wdata(0) := Mux(base_w_inc, inc_ctr(base_w, base_w_inc, 1)
        , Mux(base_w_dec, inc_ctr(base_w, !base_w_dec, 1), base_w))

    //  Update wa table
    val wa_w_inc = wa_w < 13.S & taken
    val wa_w_dec = wa_w > -13.S & !taken
    update_widx(1) := wa_id
    update_wdata(1) := Mux(wa_w_inc, inc_ctr(wa_w, wa_w_inc, 3)
        , Mux(wa_w_dec, inc_ctr(wa_w, !wa_w_dec, 3), wa_w))

    //  Update wb table
    val wb_w_inc = wb_w < 7.S & taken
    val wb_w_dec = wb_w > -7.S & !taken
    update_widx(2) := wb_id
    update_wdata(2) := Mux(wb_w_inc, inc_ctr(wb_w, wb_w_inc, 1)
        , Mux(wb_w_dec, inc_ctr(wb_w, !wb_w_dec, 1), wb_w))

    //  Update wc table
    val wc_w_inc = wc_w < 15.S & taken
    val wc_w_dec = wc_w > -15.S & !taken
    update_widx(3) := wc_id
    update_wdata(3) := Mux(wc_w_inc, inc_ctr(wc_w, wc_w_inc, 1)
        , Mux(wc_w_dec, inc_ctr(wc_w, !wc_w_dec, 1), wc_w))

    //  Updating
    for (t <- 0 until numTables) {
        tables(t).update.valid := update.valid
        tables(t).update.bits.index := update_widx(t)
        tables(t).update.bits.weight := update_wdata(t)
    }
    when (update.valid) {
        ghist := Cat(ghist(globalHistoryLength - 2, 0), update.bits.taken)
    }
}