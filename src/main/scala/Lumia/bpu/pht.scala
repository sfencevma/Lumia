package Lumia.bpu

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import scala.math.min

case class PHTParams (
    nEntries: Int = 2048
)

trait HasPhtParameters extends HasCoreParameters {
    val phtParams = tileParams.pht.get
    val nEntries = phtParams.nEntries
    val phtIdBits = log2Ceil(nEntries)
}

class PhtResp(implicit p: Parameters) extends LumiaBundle with HasPhtParameters {
    val index = UInt(width = phtIdBits)
    val status = UInt(width = 2)
}


class PhtUpdateReq(implicit p: Parameters) extends LumiaBundle with HasPhtParameters {
    val taken = Bool()
    val index = UInt(width = phtIdBits)
    val status = UInt(width = 2)
}


class PhtIO(numReadPorts: Int)(implicit p: Parameters) extends LumiaBundle with HasPhtParameters {
    val req = Vec(numReadPorts, Valid(UInt(width = vaddrBits)).flip)
    val update = Valid(new PhtUpdateReq).flip
    val resp = Vec(numReadPorts, new PhtResp().asOutput)
    val pht_index = Vec(numReadPorts, UInt(OUTPUT, width = phtIdBits))
    val pht_status = Vec(numReadPorts, UInt(OUTPUT, width = 2))
}

class Pht(numReadPorts: Int)(implicit p: Parameters) extends LumiaModule with HasPhtParameters {
    val io = IO(new PhtIO(numReadPorts))

    def compute_folded(pc: UInt, len: Int) = {
        val nChunks = (pc.getWidth + len - 1) / len
        val chunks = (0 until nChunks) map {
            i => pc(min((i + 1) * len, pc.getWidth) - 1, i * len)
        }
        chunks.reduce(_^_)
    }

    def inc_ctr(ctr: UInt, taken: Bool) = {
        Mux(taken, Mux(ctr === 3.U, ctr, ctr + 1.U)
            , Mux(ctr === 0.U, ctr, ctr - 1.U))
    }

    //*******************************************
    //  Module body
    val pht = chisel3.SyncReadMem(nEntries, UInt(width = 2))

    //*******************************************
    //  Read
    val ridx = io.req.map(s => compute_folded(s.bits, phtIdBits))
    val bypass_hit = io.req zip ridx map { case (f, s) => f.valid & io.update.valid & (s === io.update.bits.index) }
    val idx = bypass_hit.zipWithIndex.map { case (f, i) => Mux(f, io.update.bits.index, ridx(i)) }
    io.pht_status := io.req zip idx map { case (v, i) => pht.read(i, v.valid) }
    io.pht_index.zipWithIndex.map { case (f, i) => f := RegNext(idx(i)) }

    //*******************************************
    //  Update
    val newest_status = inc_ctr(io.update.bits.status, io.update.bits.taken)
    when (io.update.valid) {
        pht.write(newest_status, io.update.bits.index)
    }
}