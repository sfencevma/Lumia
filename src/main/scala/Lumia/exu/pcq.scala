package Lumia.exu

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._


class PCQIO(numReadPorts: Int)(implicit p: Parameters) extends LumiaBundle {
    val enq = Vec(dispatchWidth, Valid(new Bundle() {
        val pcq_id = UInt(width = pcqIdBits)
        val inst_addr_hi = UInt(width = vaddrBits - 12)
    }).flip)
    val req = Vec(numReadPorts, UInt(INPUT, width = pcqIdBits))
    val pcq_resp = Vec(numReadPorts, UInt(width = vaddrBits - 12))
}

class PCQ(numReadPorts: Int)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new PCQIO(numReadPorts))

    def getIdx(id: UInt) = id(pcqIdBits - 2, 0)

    val pcq = Reg(Vec(numPcqEntries, UInt((vaddrBits - 12).W)))

    for (dw <- 0 until dispatchWidth) {
        when(io.enq(dw).valid) {
            pcq(getIdx(io.enq(dw).bits.pcq_id)) := io.enq(dw).bits.inst_addr_hi
        }
    }

    for (i <- 0 until numReadPorts) {
        io.pcq_resp(i) := pcq(getIdx(io.req(i)))
    }
}
