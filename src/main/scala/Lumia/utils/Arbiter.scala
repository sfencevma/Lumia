package Lumia.utils

import Chisel._

class PriorityArbiter[T <: Data](val gen: T, val n: Int) extends Module {
    val io = IO(new ArbiterIO(gen, n))

    val arb = Module(new RRArbiter(gen, n - 1))

    for (i <- 0 until n - 1) {
        arb.io.in(i) <> io.in(i + 1)
        arb.io.in(i).valid := !io.in(0).valid && io.in(i + 1).valid
    }

}