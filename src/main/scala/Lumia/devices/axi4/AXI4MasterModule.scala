package Lumia.devices.axi4

import Chisel._
import freechips.rocketchip.amba.axi4._
import Lumia.utils._

abstract class AXI4MasterModule[B <: Data](params: AXI4BundleParameters, _outer: B = null) extends Module {
    val io = IO(new Bundle() {
        val axi = new AXI4Bundle(params)
        val outer = if (_outer != null) Some(_outer) else None
    })
}
