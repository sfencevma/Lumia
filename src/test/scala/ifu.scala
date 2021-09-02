package Lumia

import Chisel._
import Lumia.common._
import org.scalatest._
import chisel3.iotesters._
import freechips.rocketchip.config.Parameters
import Lumia.fetch._
import freechips.rocketchip.amba.axi4.AXI4BundleParameters

class ifu_tester(dut: Fetch) extends PeekPokeTester(dut) {
    step(1)

    step(10)
}


class ifu_spec extends FlatSpec with Matchers {

    val cloverParams: Parameters = LumiaTestUnit.getLumiaParameters("LumiaConfig")
    implicit val p: Parameters = cloverParams.alterPartial {
        case LumiaTileKey => cloverParams(LumiaTileKey)
    }
    "ifu_top" should "pass" in {
        chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"),() => new Fetch()) {
            c => new ifu_tester(c)
        } should be (true)
    }
}