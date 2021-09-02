package Lumia.perips

import Chisel._

import freechips.rocketchip.amba.axi4._

import Lumia.devices.axi4._

import scala.collection.mutable.LinkedHashMap


object ClintConsts {
    def timeOffset = 0xbff8
    def msipBytes = 4
    def timecmpBytes = 8
    def size = 0x10000
    def timeWidth = 64
    def ipiWidth = 32
    def ints = 2

    val msip = 0x02000000
    val timecmp_lo = 0x02004000
    val timecmp_hi = 0x02004004
    val time_lo = 0x0200bff8
    val time_hi = 0x0200bffc

    def msipOffset(hart: Int) = hart * msipBytes
    def timecmpOffset(hart: Int) = 0x4000 + hart * timecmpBytes
}

class ClintIO extends Bundle {
    val rtcTick = Bool(INPUT)
    val timer_int = Bool(OUTPUT)
    val sft_int = Bool(OUTPUT)
}

class Clint(params: AXI4BundleParameters) extends AXI4SlaveModule(params, new ClintIO) {
    val outer = io.outer.get

    val time = Reg(init = UInt(0, width = ClintConsts.timeWidth))
    val timecmp_lo = Reg(UInt(width = 32))
    val timecmp_hi = Reg(UInt(width = 32))
    val msip = Reg(init = UInt(0, width = ClintConsts.ipiWidth))

    //  Interrupts
    when (outer.rtcTick) { time := time + 1.U }
    outer.sft_int := msip(0)
    outer.timer_int := time.asUInt() >= Cat(timecmp_hi, timecmp_lo).asUInt()

    //
    val read_mapping = LinkedHashMap[Int, Bits] (
        0x02000000 -> msip,
        0x02004000 -> timecmp_lo,
        0x02004004 -> timecmp_hi,
        0x0200bff8 -> time(31, 0),
        0x0200bffc -> time(ClintConsts.timeWidth - 1, 32)
    )
    //  Read
    val r_decoded_addr = read_mapping map { case (k, v) => k -> (io.axi.ar.bits.addr === k.U)}
    r_bits.id := ar_bits.id
    r_bits.data := Mux1H(for ((k, v) <- read_mapping) yield r_decoded_addr(k) -> v)
    r_bits.resp := AXI4Parameters.RESP_OKAY
    r_bits.last := read_ctr === 0.U

    //  Write
    b_bits.id := aw_bits.id
    b_bits.resp := AXI4Parameters.RESP_OKAY

    val w_decoded_addr = read_mapping map { case (k, v) => k -> (io.axi.aw.bits.addr === k.U)}
    when (mem_wr_en) {
        when (w_decoded_addr(ClintConsts.msip)) {
            msip := Cat(Fill(31, 0.U), io.axi.w.bits.data(0))
        }
        when (w_decoded_addr(ClintConsts.timecmp_lo)) {
            timecmp_lo := io.axi.w.bits.data
        }
        when (w_decoded_addr(ClintConsts.timecmp_hi)) {
            timecmp_hi := io.axi.w.bits.data
        }
    }
}