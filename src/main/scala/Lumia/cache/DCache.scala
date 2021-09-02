package Lumia.cache

import Chisel._
import freechips.rocketchip.config.Parameters
import Lumia.utils._
import Lumia.common._

case class DCacheParams (
    nSets: Int = 256,
    nWays: Int = 4,
    rowBits: Int = 4,
    nTLBSets: Int = 8,
    nTLBWays: Int = 16,
    blockBits: Int = 512
) extends L1CacheParams {
    def genIndex(pc: UInt) = pc(13, 6)
    def genTag(pc: UInt) = pc(33, 14)
    def replacement = new PseudoLRU(nWays)
}

trait HasL1DCacheParameters extends HasL1CacheParameters with HasCoreParameters {
    val cacheParams = tileParams.dcache.get
}

class DCache(implicit p: Parameters) extends Cache(needWT = true) with HasL1DCacheParameters {
    override val nWays: Int = cacheParams.nWays
    override val nSets: Int = cacheParams.nSets
    override val tagBits: Int = 20
    override val dataBits: Int = cacheParams.blockBits
    override def genIndex(pc: UInt): UInt = cacheParams.genIndex(pc)
    override def genTag(pc: UInt): UInt = cacheParams.genTag(pc)
}