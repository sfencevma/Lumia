package Lumia.devices.axi4

import Chisel._
import freechips.rocketchip.amba.axi4._

class AXI4RAM(params: AXI4BundleParameters) extends AXI4SlaveModule(params) {

    def memBytes = params.dataBits / 8
    def offsetBits = log2Up(memBytes)
    def offsetMask = (1 << offsetBits) - 1
    def index(addr: UInt) = ((addr & offsetMask.U) >> log2Ceil(memBytes)).asUInt()

    val ram = chisel3.SyncReadMem(256, Vec(params.dataBits / 8, UInt(8.W)))

    //  Read
    r_bits.id   := ar_bits.id
    r_bits.data := Cat(ram(index(read_addr)).reverse)
    r_bits.resp := AXI4Parameters.RESP_OKAY
    r_bits.last := read_ctr === 0.U

    //  Write
    b_bits.id := aw_bits.id
    b_bits.resp := AXI4Parameters.RESP_OKAY

    val wdata = Vec.tabulate(memBytes) { i => io.axi.w.bits.data(8 * i + 7, 8 * i) }
    when (mem_wr_en) {
        ram.write(index(aw_bits.addr), wdata, io.axi.w.bits.strb.asBools())
    }
}