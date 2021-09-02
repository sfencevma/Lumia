package Lumia

import Chisel._
import Lumia.common._
import org.scalatest._
import chisel3.iotesters._
import freechips.rocketchip.config.Parameters
import Lumia.mmu._
import freechips.rocketchip.amba.axi4.AXI4BundleParameters

class mmu_tester(dut: MMU) extends PeekPokeTester(dut) {
    step(1)
    poke(dut.io.outer.get.itlb_ptw.req.valid, true.B)
    poke(dut.io.outer.get.itlb_ptw.req.bits.addr, "hfffff".U)
    step(10)
}

class mmu_spec extends FlatSpec with Matchers {
    val params = new AXI4BundleParameters(addrBits = 32,
        dataBits = 32,
        idBits = 1
    )
    val cloverParams: Parameters = LumiaTestUnit.getLumiaParameters("LumiaConfig")
    implicit val p: Parameters = cloverParams.alterPartial {
        case LumiaTileKey => cloverParams(LumiaTileKey)
    }
    "mmu_top" should "pass" in {
        chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"),() => new MMU(params)) {
            c => new mmu_tester(c)
        } should be (true)
    }
}