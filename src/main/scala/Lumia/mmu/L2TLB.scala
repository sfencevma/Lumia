package Lumia.mmu

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.common._
import Lumia.cache._
import Lumia.utils._

case class L2TLBParams (
    nSets: Int = 256,
    nWays: Int = 4
) {
    def replacement = new PseudoLRU(nWays)
}

trait HasL2TLBParameters extends HasTileParameters with HasCoreParameters {
    val l2tlbParams = tileParams.l2tlb.get
    val nSets = l2tlbParams.nSets
    val nWays = l2tlbParams.nWays
}

class L2TLBModule(nWays: Int, nSets: Int)(implicit p: Parameters) extends TLB(nWays, nSets)
class L2TLB(implicit p: Parameters) extends LumiaBundle with HasL2TLBParameters {
    val module = new L2TLBModule(nWays, nSets)
}