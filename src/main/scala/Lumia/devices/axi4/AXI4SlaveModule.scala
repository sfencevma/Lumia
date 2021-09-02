package Lumia.devices.axi4

import Chisel._
import freechips.rocketchip.amba.axi4._
import Lumia.utils._

abstract class AXI4SlaveModule[B <: Data](params: AXI4BundleParameters, _outer: B = null) extends Module {
    val io = IO(new Bundle() {
        val axi = new AXI4Bundle(params).flip
        val outer = if (_outer != null) Some(_outer) else None
        val kill = Bool(INPUT)
    })

    //  Write
    //  AW port
    val aw_hsk = io.axi.aw.fire()
    val aw_bits = HoldUnless(io.axi.aw.bits, aw_hsk)
    val aw_ready = Reg(init = Bool(false))

    //  W port
    val w_hsk = io.axi.w.fire()
    val w_ready = Reg(init = Bool(false))

    //  B Port
    val b_hsk = io.axi.b.fire()
    val b_bits = Wire(new AXI4BundleB(params))
    val reg_b_bits = Reg(new AXI4BundleB(params))
    val b_valid = Reg(init = Bool(false))

    //  Write Finite state machine
    val s_idle::s_burst::s_resp::Nil = Enum(3)
    val w_state = Reg(init = s_idle)

    //
    val write_addr = Reg(UInt(params.addrBits.W))
    val write_ctr  = Reg(UInt(params.lenBits.W))

    //  Set default
    val mem_wr_en = w_hsk
    aw_ready := false.B
    b_valid := b_valid & !io.axi.b.ready


    switch (w_state) {
        is (s_idle) {
            aw_ready := true.B
            when (aw_hsk) {
                aw_ready := false.B
                w_ready := true.B
                write_addr := io.axi.aw.bits.addr
                write_ctr := io.axi.aw.bits.len
                w_state := s_burst
            }
        }
        is (s_burst) {
            when (w_hsk) {
                when (aw_bits.burst =/= AXI4Parameters.BURST_FIXED) {
                    write_addr := write_addr + (1.U << aw_bits.size)
                }
                write_ctr := write_ctr - 1.U
                when (write_ctr === 0.U) {
                    w_ready := false.B
                    when (io.axi.b.ready || !b_valid) {
                        b_valid := true.B
                        aw_ready := true.B
                        reg_b_bits := b_bits
                        w_state := s_idle
                    }
                } .otherwise {
                    w_ready := true.B
                    w_state := s_burst
                }
            }
        }
        is (s_resp) {
            when (io.axi.b.ready || !b_valid) {
                b_valid := true.B
                aw_ready := true.B
                w_state := s_idle
            }
        }
    }

    io.axi.aw.ready     := aw_ready
    io.axi.w.ready      := w_ready
    io.axi.b.valid      := b_valid
    io.axi.b.bits       := reg_b_bits

    when (io.kill) { w_state := s_idle }

    //  Read Finite state machine
    //  AR port
    val ar_hsk = io.axi.ar.valid && io.axi.ar.ready
    val ar_bits = HoldUnless(io.axi.ar.bits, ar_hsk)
    val ar_ready = Reg(init = Bool(false))

    //  R port
    val r_valid = RegInit(false.B)
    val r_bits = Wire(new AXI4BundleR(params))
    val reg_r_bits = Reg(new AXI4BundleR(params))

    val r_state = Reg(init = s_idle)
    val read_addr = Reg(UInt(params.addrBits.W))
    val read_ctr  = Reg(UInt(params.lenBits.W))

    //  Set default
    val mem_rd_en = io.axi.r.ready || !r_valid

    ar_ready := false.B
    r_valid := r_valid && !io.axi.r.ready

    switch (r_state) {
        is (s_idle) {
            ar_ready := true.B
            when (ar_hsk) {
                read_addr := io.axi.ar.bits.addr
                read_ctr := io.axi.ar.bits.len
                ar_ready := false.B
                r_state := s_burst
            }
        }
        is (s_burst) {
            when (io.axi.r.ready || !r_valid) {
                r_valid := true.B
                reg_r_bits := r_bits
                when (ar_bits.burst =/= AXI4Parameters.BURST_FIXED) {
                    read_addr := read_addr + (1.U << ar_bits.size)
                }
                read_ctr := read_ctr - 1.U
                when (read_ctr === 0.U) {
                    ar_ready := true.B
                    r_state := s_idle
                }
            }
        }
    }

    io.axi.ar.ready     := ar_ready
    io.axi.r.valid      := r_valid
    io.axi.r.bits       := reg_r_bits

    when (io.kill) { r_state := s_idle }
}