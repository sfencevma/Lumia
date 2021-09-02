package Lumia.perips

import Chisel._

import freechips.rocketchip.amba.axi4._

import Lumia.devices.axi4._
import Lumia.utils._

object PLIC {
    val nWdogs = 1
    val nRtcs = 1
    val nUarts = 2
    val nQspis = 3
    val nGpios = 32
    val nPwms = 12

    val maxDevices = 1023
    val maxHarts = 15872
    val nDevices = nWdogs + nRtcs + nUarts + nQspis + nGpios + nPwms
    val nHarts = 1

    val priorityBase = 0x0
    val pendingBase = 0x1000
    val enableBase = 0x2000
    val thresholdBase = 0x200000
    val claimCompleteBase = 0x200004


    def enableOffset(i: Int) = i * 0x80
    def enableBase(i: Int, w: Int): Int = enableBase + enableOffset(i) + 4 * w
    def thresholdOffset(i: Int): Int = i * 0x1000
    def thresholdBase(i: Int): Int = thresholdBase + thresholdOffset(i)
    def claimCompleteOffset(i: Int): Int = i * 0x1000
    def claimCompleteBase(i: Int): Int = claimCompleteBase + claimCompleteOffset(i)
}

class GatewaysIO extends Bundle {
    val valid = Output(Bool())
    val ready = Input(Bool())
    val complete = Input(Bool())
}

class LevelGateway extends Module {
    val io = IO(new Bundle() {
        val interrupt = Input(Bool())
        val plic = new GatewaysIO
    })

    val int_req = Reg(init = false.B)
    when (io.interrupt & io.plic.ready) { int_req := true.B }
    when (io.plic.complete) { int_req := false.B }
    io.plic.valid := io.interrupt && !int_req
}

class PLICIO extends Bundle {
    val wdogcmp = Vec(PLIC.nWdogs, Flipped(new GatewaysIO))
    val rtccmp  = Vec(PLIC.nRtcs, Flipped(new GatewaysIO))
    val uart    = Vec(PLIC.nUarts, Flipped(new GatewaysIO))
    val qspi    = Vec(PLIC.nQspis, Flipped(new GatewaysIO))
    val gpio    = Vec(PLIC.nGpios, Flipped(new GatewaysIO))
    val pwm     = Vec(PLIC.nPwms, Flipped(new GatewaysIO))
}

class Plic(params: AXI4BundleParameters) extends AXI4SlaveModule(params, new PLICIO) {
    val outer = io.outer.get

    def nDevices = PLIC.nDevices

    def nHarts = PLIC.nHarts

    def minPriorities = nDevices max 7

    def nPriorities = (1 << log2Ceil(minPriorities + 1)) - 1

    def addressSpaceSize = 0x4000000

    def addressBits = log2Up(addressSpaceSize)

    def getOffset(addr: UInt) = addr(addressBits - 1, 0)

    val interrupts = outer.wdogcmp ++ outer.rtccmp ++ outer.uart ++ outer.qspi ++ outer.gpio ++ outer.pwm
    val gateways = interrupts.map { case i =>
        val gateway = Module(new LevelGateway)
        gateway.io.interrupt := i.valid
        i.ready := gateway.io.plic.ready
        i.complete := gateway.io.plic.complete
        gateway.io.plic
    }

    //  Priority field
    def priorityRegField(x: UInt): RegMapField = {
        if (nPriorities > 0) {
            RegMapField(32, x)
        } else {
            RegMapField.r(32, x)
        }
    }

    val priority = Reg(Vec(nDevices, UInt(width = 32)))
    val priorityRegFields = priority.zipWithIndex.map { case (p, i) =>
        PLIC.priorityBase + 4 * (i + 1) -> priorityRegField(p)
    }

    //  Pending field
    val devs = (nDevices + 31) / 32
    val pending = Reg(init = Vec.fill(devs)(UInt(0, width = 32)))
    val pendingRegFields = pending.zipWithIndex.map { case (p, i) =>
        PLIC.pendingBase + 4 * i -> RegMapField.r(32, p)
    }

    //  Enable field
    def enableRegs = List.fill(devs)(Reg(init = UInt(0, width = 32)))

    val enables = Seq.fill(nHarts) {
        enableRegs
    }
    val enableVec = Vec(enables.map(x => Cat(x)))
    val enableVec0 = Vec(enableVec.map(x => Cat(x, UInt(0, width = 1))))
    val enableRegFields = enables.zipWithIndex.map { case (f, hart) =>
        f.zipWithIndex.map { case (r, w) => PLIC.enableBase(hart, w) -> RegMapField(32, r) }
    }.reduce(_ ++ _).toMap

    //  Threshold
    val threshold = Reg(Vec(nHarts, UInt(width = 32)))

    def thresholdRegField(x: UInt) =
        if (nPriorities > 0) {
            RegMapField(32, x)
        } else {
            RegMapField.r(32, x)
        }

    val thresholdRegFields = Seq.tabulate(nHarts) { i =>
        PLIC.thresholdBase(i) -> thresholdRegField(threshold(i))
    }

    //
    val harts = Reg(Vec(nHarts, Bool()))
    val maxDevs = Wire(Vec(nHarts, UInt(width = log2Ceil(nDevices + 1))))
    val pendingVec = Cat(pending.map(x => x))
    maxDevs.zipWithIndex.map { case (r, hart) => {
        val takenVec = pendingVec & Cat(enables(hart))
        r := Mux(takenVec === 0.U, UInt(0), PriorityEncoder(takenVec))
    }
    }

    //
    val claimer = Wire(Vec(nHarts, Bool()))
    val claiming = Seq.tabulate(nHarts) { i => Mux(claimer(i), maxDevs(i), UInt(0)) }.reduce(_ | _)
    val claimedDevs = Vec(UIntToOH(claiming, nDevices + 1).asBools())
    val completer = Wire(Vec(nHarts, Bool()))
    val completerDev = Wire(UInt(width = log2Up(nDevices + 1)))
    val completedDevs = Mux(completer.reduce(_ || _), UIntToOH(completerDev, nDevices + 1), UInt(0))

    val claimCompleteRegFields = Seq.tabulate(nHarts) { i =>
        PLIC.claimCompleteBase(i) -> RegMapField(32,
            ReadFn {
                valid =>
                    claimer(i) := valid
                    (Bool(true), maxDevs(i))
            }
            , WriteFn {
                (valid, data) =>
                    completerDev := data(log2Ceil(nDevices + 1) - 1, 0)
                    completer(i) := valid && enableVec0(i)(completerDev)
                    Bool(true)
            })
    }

    (gateways zip completedDevs.asBools().tail) foreach {
        case (g, c) =>
            g.complete := c
    }
    ((pending zip gateways) zip claimedDevs.tail) foreach {
        case ((p, g), c) =>
            g.ready := !p
            when(c || g.valid) {
                p := !c
            }
    }

    val regMap = priorityRegFields ++ pendingRegFields ++ enableRegFields ++ thresholdRegFields ++ claimCompleteRegFields

    interrupts.map(i => i.valid).zipWithIndex.map { case (intr, i) =>
        val id = i + 1
        when(intr) {
            pending(id / 32) := pending(id / 32).bitSet((id % 32).U, true.B)
        }
    }

    //  Read
    val rSLVERR = mem_rd_en & !regMap.map(p => (p._1.U === getOffset(read_addr))).reduce(_ | _)
    val rdata = Mux1H(regMap.map(p => (p._1.U === getOffset(read_addr), p._2.readFn.fn(true.B, true.B)._3)))

    //  Write
    val wmask = FillInterleaved(8, io.axi.w.bits.strb)
    regMap.map { case (a, r) =>
        r.writeFn.fn(mem_wr_en, getOffset(write_addr) === a.U, MaskData(r.readFn.fn(true.B, true.B)._3, io.axi.w.bits.data, wmask))
    }
    val wSLVERR = mem_wr_en & !regMap.map(p => (p._1.U === getOffset(write_addr))).reduce(_ | _)
    //  Write
    b_bits.id := aw_bits.id
    b_bits.resp := Mux(wSLVERR, AXI4Parameters.RESP_SLVERR, AXI4Parameters.RESP_OKAY)
    //  Read
    r_bits.id := ar_bits.id
    r_bits.data := rdata
    r_bits.last := read_ctr === UInt(0)
    r_bits.resp := Mux(rSLVERR, AXI4Parameters.RESP_SLVERR, AXI4Parameters.RESP_OKAY)
}