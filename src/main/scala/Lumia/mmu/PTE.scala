package Lumia.mmu

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import Lumia.utils._

class PTE(implicit p: Parameters) extends LumiaBundle {
    val ppn = UInt(width = 22)
    val rsw = UInt(width = 2)
    val d = Bool()
    val a = Bool()
    val g = Bool()
    val u = Bool()
    val x = Bool()
    val w = Bool()
    val r = Bool()
    val v = Bool()

    def table(dummy: Int = 0) = v && !r && !w && !x
    def leaf(dummy: Int = 0) = v && (r || (x && !w)) && a
    def sr(dummy: Int = 0) = leaf() && r
    def ur(dummy: Int = 0) = sr() && u
    def sw(dummy: Int = 0) = leaf() && w && d
    def uw(dummy: Int = 0) = sw() && u
    def sx(dummy: Int = 0) = leaf() && x
    def ux(dummy: Int = 0) = sx() && u
    def getWidth(dummy: Int = 0) = 32
}

