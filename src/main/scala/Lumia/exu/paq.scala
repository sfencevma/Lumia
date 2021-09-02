package Lumia.exu

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._


class PAQIO(numReadPorts: Int)(implicit p: Parameters) extends LumiaBundle {
    val enq = Vec(dispatchWidth, Valid(new Bundle() {
        val paq_id = UInt(width = paqIdBits)
        val inst_taddr = UInt(width = vaddrBits)
    }).flip)
    val req = Vec(numReadPorts, UInt(INPUT, width = paqIdBits))
    val resp = Vec(numReadPorts, UInt(OUTPUT, width = vaddrBits))
}

class PAQ(numReadPorts: Int)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new PAQIO(numReadPorts))
    def getIdx(id: UInt) = id(paddrBits - 2, 0)
    val paq = Reg(Vec(numPaqEntries, UInt(width = vaddrBits)))

    for (dw <- 0 until dispatchWidth) {
        when (io.enq(dw).valid) {
            paq(getIdx(io.enq(dw).bits.paq_id)) := io.enq(dw).bits.inst_taddr
        }
    }

    for (i <- 0 until numReadPorts) {
        io.resp(i) := paq(getIdx(io.req(i)))
    }
}