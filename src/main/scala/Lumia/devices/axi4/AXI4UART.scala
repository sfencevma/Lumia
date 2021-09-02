package Lumia.devices.axi4

import Chisel._
import freechips.rocketchip.amba.axi4._


class AXI4UARTIO extends Bundle {
    val rxd = Bool(INPUT)
    val txd = Bool(OUTPUT)

    val tx_busy =Bool()
    val rx_busy = Bool(OUTPUT)
    val rx_overrun_error = Bool(OUTPUT)
    val rx_frame_error = Bool(OUTPUT)

    val prescale = UInt(INPUT, 16.W)
}

class AXI4UART(params: AXI4BundleParameters) extends AXI4SlaveModule(params, new AXI4UARTIO) {
    val outer = io.outer.get
    //  RX port
    val tdata = Reg(init = UInt(0, width = params.dataBits))
    val tvalid = Reg(init = Bool(false))
    val rxd = Reg(init = Bool(true))
    val r_prescale = Reg(init = UInt(0, width = 19))
    val r_bit_ctr = Reg(init = UInt(0, width = 4))
    val r_busy = Reg(init = Bool(false))
    val overrun_error = Reg(init = Bool(false))
    val frame_error = Reg(init = Bool(false))

    rxd := outer.rxd
    overrun_error := false.B
    frame_error := false.B

    when (io.axi.r.ready && io.axi.r.valid) {
        tvalid := false.B
    }

    when (r_prescale > 0.U) {
        r_prescale := r_prescale - 1.U
    } .elsewhen (r_bit_ctr > 0.U) {
        when (r_bit_ctr > (params.dataBits + 1).U) {
            when  (!rxd) {
                r_bit_ctr := r_bit_ctr - 1.U
                r_prescale := (outer.prescale << 3.U) - 1.U
            } .otherwise {
                r_bit_ctr := 0.U
                r_prescale := 0.U
            }
        } .elsewhen (r_bit_ctr > 1.U) {
            r_bit_ctr := r_bit_ctr - 1.U
            r_prescale := (outer.prescale << 3.U) - 1.U
            tdata := Cat(rxd, tdata(params.dataBits - 1, 1))
        } .elsewhen (r_bit_ctr === 1.U) {
            r_bit_ctr := r_bit_ctr - 1.U
            when (rxd) {
                tvalid := true.B
                overrun_error := tvalid
            } .otherwise {
                frame_error := true.B
            }
        }
    } .otherwise {
        r_busy := false.B
        when (!rxd) {
            r_prescale := (outer.prescale << 2.U) - 2.U
            r_bit_ctr := params.dataBits.U + 2.U
            tdata := 0.U
            r_busy := true.B
        }
    }

    io.axi.r.valid := tvalid
    io.axi.r.bits.data := tdata

    outer.rx_busy := r_busy
    outer.rx_frame_error := frame_error
    outer.rx_overrun_error := overrun_error


    //  TX  port
    val tready = RegInit(false.B)
    val txd = RegInit(true.B)
    val t_busy = RegInit(false.B)
    val t_prescale = RegInit(0.U(19.W))
    val t_data = RegInit(0.U((params.dataBits + 1).W))
    val t_bit_ctr = RegInit(0.U(4.W))

    txd := true.B
    when (t_prescale > 0.U) {
        tready := false.B
        t_prescale := t_prescale - 1.U
    } .elsewhen (t_bit_ctr === 0.U) {
        tready := true.B
        t_busy := false.B

        when (io.axi.aw.valid) {
            tready := !tready
            t_prescale := (outer.prescale << 3.U) - 1.U
            t_bit_ctr := (params.dataBits + 1).U
            t_data := Cat(1.U(1.W), io.axi.w.bits.data)
            txd := false.B
            t_busy := true.B
        }
    } .otherwise {
        when (t_bit_ctr > 1.U) {
            t_bit_ctr := t_bit_ctr - 1.U
            t_prescale := (outer.prescale << 3.U) - 1.U
            txd := Cat(0.U(1.W), t_data)(0)
            t_data := Cat(0.U(1.W), t_data)(params.dataBits + 2, 1)
        } .elsewhen (t_bit_ctr === 1.U) {
            t_bit_ctr := t_bit_ctr - 1.U
            t_prescale := outer.prescale << 3.U
            txd := true.B
        }
    }

    io.axi.w.ready := tready
    outer.tx_busy := t_busy
    outer.txd := txd
}
