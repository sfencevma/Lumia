package Lumia.common

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import Lumia.common._

import freechips.rocketchip.rocket._

import Lumia.bpu._
import Lumia.cache._
import Lumia.iq._
//*****************************
//  Core configs
class WithNMegaLumia(n: Int = 1) extends Config(
    new Config((site, here, up) => {
        case LumiaTileKey => {
            LumiaTileParams (
                core = LumiaCoreParams(),
                icache = Some(Lumia.cache.ICacheParams()),
                dcache = Some(Lumia.cache.DCacheParams()),
                btb = Some(Lumia.bpu.BTBParams()),
                pht = Some(Lumia.bpu.PHTParams()),
                btac = Some(Lumia.bpu.BTACParams()),
                l2tlb = Some(Lumia.mmu.L2TLBParams()),
                ptecache = Some(Lumia.mmu.PTECacheParams()),
                hartId = 0
                // btb = Some(BTBParams())
                //       l2tlb = Some(L2TLBParams()),
                //       pte_cache = Some(PTECacheParams),
                //       btac = Some(BTACParams),
                //      pht = Some(PHTParams),
            )
        }
        case XLen => 32
        case ASIdBits => 9
        case PgLevels => 2
    })
)