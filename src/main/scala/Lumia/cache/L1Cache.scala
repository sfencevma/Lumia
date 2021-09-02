package Lumia.cache

import Chisel._
import Lumia.common._
import freechips.rocketchip.config.Parameters


trait L1CacheParams {
    def nSets: Int
    def nWays: Int
    def rowBits: Int
    def nTLBSets: Int
    def nTLBWays: Int
    def blockBits: Int
}

trait HasL1CacheParameters extends HasTileParameters {
    val cacheParams: L1CacheParams

    def nSets = cacheParams.nSets
    def nWays = cacheParams.nWays
    def rowBits = cacheParams.rowBits
    def rowBytes= rowBits / 8
    def nTLBSets= cacheParams.nTLBSets
    def nTLBWays= cacheParams.nTLBWays
    def blockBits = cacheParams.blockBits
    def blockBytes = blockBits / 8

    def blockOffBits = log2Up(blockBytes)
    def idxBits = log2Up(nSets)
    def wayBits = log2Up(nWays)
    def untagBits = blockOffBits + idxBits
    def tagBits = paddrBits - untagBits

    def cacheDataBits = blockBytes * 8
    def cacheDataBytes = blockBytes
    def cacheDataBeats = cacheDataBytes / memDataBytes
    def refillCycles = cacheDataBeats
}

case class L1CacheParameters (
                               nSets: Int = 256,
                               nWays: Int = 4,
                               rowBits: Int = 4,
                               nTLBSets: Int = 8,
                               nTLBWays: Int = 16,
                               blockBits: Int = 512
                             ) extends L1CacheParams

abstract class L1CacheModule(implicit val p: Parameters) extends Module
  with HasL1CacheParameters
abstract class L1CacheBundle(implicit val p: Parameters) extends Bundle
  with HasL1CacheParameters