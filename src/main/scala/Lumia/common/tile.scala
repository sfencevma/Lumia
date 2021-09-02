package Lumia.common

import freechips.rocketchip.config._
import Lumia.bpu._
import Lumia.cache._
import Lumia.mmu._


case class LumiaTileParams (
    core: LumiaCoreParams,
    icache: Option[ICacheParams],
    dcache: Option[DCacheParams],
    btb: Option[BTBParams],
    pht: Option[PHTParams],
    btac: Option[BTACParams],
    l2tlb: Option[L2TLBParams],
    ptecache: Option[PTECacheParams],
    hartId: Int,
    //val name: Option[String]
)

class LumiaTile private (
    val LumiaParams: LumiaTileParams,
    p: Parameters
) {

}
case class LumiaTileAttachParams (
    tileParams: LumiaTileParams
) {
    type tiletype = LumiaTile
}

case object LumiaTileKey extends Field[LumiaTileParams]