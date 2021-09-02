package Lumia.mmu

import Chisel._
import Lumia.common._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util._

class PMPConfigReg extends Bundle {
    val l = Bool()
    val res = UInt(0, width = 2)
    val a = UInt(width = 2)
    val x = Bool()
    val w = Bool()
    val r = Bool()
}

object PMP {
    def align = 2
    def apply(reg: PMPReg): PMP = {
        val pmp = Wire(new PMP()(reg.p))
        pmp.cfg := reg.cfg
        pmp.addr := reg.addr
        pmp.mask := pmp.computeMask
        pmp
    }
}

class PMPReg(implicit p: Parameters) extends LumiaBundle {
    val cfg = new PMPConfigReg
    val addr = UInt(width = paddrBits - PMP.align)

    def napot   = cfg.a === 3.U
    def na4     = cfg.a === 2.U
    def tor     = cfg.a === 1.U
    def off     = cfg.a === 0.U
    def torNotNAPOT = cfg.a(0)
    def locked  = cfg.l
    def addrLocked(next: PMPReg) = locked || next.locked && next.tor

    def reset() = {
        cfg.a := 0.U
        cfg.l := false.B
    }

    def readAddr = if (log2Ceil(pmpGranularity) == PMP.align) {
        addr
    } else {
        val mask = ((BigInt(1) << (log2Ceil(pmpGranularity) - PMP.align)) - 1).asUInt()
        Mux(napot, addr | (mask >> 1), ~(~addr.asUInt() | mask))
    }
}

class PMP(implicit p: Parameters) extends PMPReg {
    import PMP._
    val mask = UInt(width = paddrBits)

    def computeMask = {
        val base = Cat(addr, cfg.a(0)) | ((pmpGranularity - 1) >> align)
        Cat(base & ~(base + 1), ((1 << align) - 1).asUInt())
    }
    private def comparand: UInt = ~(~(addr << align) | (pmpGranularity - 1))
    //  Match
    private def pow2Match(x: UInt, size: UInt, maxSize: Int) = {
        def eval(a: UInt, b: UInt, m: UInt) = ((a ^ b) & ~m) === 0.U
        if (maxSize <= log2Ceil(pmpGranularity)) {
            eval(x, comparand, mask)
        } else {
            val lsbMask = mask | UIntToOH1(size, maxSize)
            val msbMatch = eval(x >> maxSize, comparand >> maxSize, mask >> maxSize)
            val lsbMatch = eval(x(maxSize - 1, 0), comparand >> maxSize, mask >> maxSize)
            msbMatch && lsbMatch
        }
    }
    private def boundMatch(x: UInt, lsbMask: UInt, maxSize: Int) = {
        if (maxSize <= log2Ceil(pmpGranularity)) {
            x < comparand
        } else {
            val msbsLess = (x >> maxSize).asUInt() < (comparand >> maxSize).asUInt()
            val msbsEqual = ((x >> maxSize).asUInt() ^ (comparand >> maxSize).asUInt()) === 0.U
            val lsbsLess =  (x(maxSize-1, 0) | lsbMask) < comparand(maxSize - 1, 0)
            msbsLess || (msbsEqual && lsbsLess)
        }
    }
    private def lowerBoundMatch(x: UInt, size: UInt, maxSize: Int) =
        !boundMatch(x, UIntToOH1(size, maxSize), maxSize)
    private def upperBoundMatch(x: UInt, lgMaxSize: Int) =
        boundMatch(x, 0.U, lgMaxSize)
    private def rangeMatch(x: UInt, lgSize: UInt, lgMaxSize: Int, prev: PMP) =
        prev.lowerBoundMatch(x, lgSize, lgMaxSize) && upperBoundMatch(x, lgMaxSize)
    private def pgLevelMap[T](f: Int => T) = (0 until pgLevels).map { i =>
        f(pgIdxBits + (pgLevels - 1 - i) * pgLevelBits)
    }
    private def pow2Homogeneous(x: UInt, pgLevel: UInt) = {
        val maskHomogeneous = pgLevelMap { idxBits => if (idxBits > paddrBits) false.B else mask(idxBits - 1) } (pgLevel)
        maskHomogeneous || (pgLevelMap { idxBits => ((x ^ comparand) >> idxBits).asUInt() =/= 0.U } (pgLevel))
    }
    private def rangeHomogeneous(x: UInt, pgLevel: UInt, prev: PMP) = {
        val beginsAfterLower = !(x < prev.comparand)
        val beginsAfterUpper = !(x < comparand)

        val pgMask = pgLevelMap { idxBits => (((BigInt(1) << paddrBits) - (BigInt(1) << idxBits)) max 0).U } (pgLevel)
        val endsBeforeLower = (x & pgMask) < (prev.comparand & pgMask)
        val endsBeforeUpper = (x & pgMask) < (comparand & pgMask)

        endsBeforeLower || beginsAfterUpper || (beginsAfterLower && endsBeforeUpper)
    }

    def homogeneous(x: UInt, pgLevel: UInt, prev: PMP): Bool = {
        Mux(napot, pow2Homogeneous(x, pgLevel), !torNotNAPOT || rangeHomogeneous(x, pgLevel, prev))
    }
    def aligned(x: UInt, lgSize: UInt, lgMaxSize: Int, prev: PMP): Bool = if (lgMaxSize <= pmpGranularity.log2) true.B else {
        val lsbMask = UIntToOH1(lgSize, lgMaxSize)
        val straddlesLowerBound = ((x >> lgMaxSize).asUInt() ^ (prev.comparand >> lgMaxSize).asUInt()) === 0.U && (prev.comparand(lgMaxSize-1, 0) & ~x(lgMaxSize-1, 0)) =/= 0.U
        val straddlesUpperBound = ((x >> lgMaxSize).asUInt() ^ (comparand >> lgMaxSize).asUInt()) === 0.U && (comparand(lgMaxSize-1, 0) & (x(lgMaxSize-1, 0) | lsbMask)) =/= 0.U
        val rangeAligned = !(straddlesLowerBound || straddlesUpperBound)
        val pow2Aligned = (lsbMask & ~mask(lgMaxSize-1, 0)) === 0
        Mux(napot, pow2Aligned, rangeAligned)
    }
    def hit(x: UInt, lgSize: UInt, lgMaxSize: Int, prev: PMP): Bool =
        Mux(napot, pow2Match(x, lgSize, lgMaxSize), torNotNAPOT && rangeMatch(x, lgSize, lgMaxSize, prev))
}

class PMPHomogeneityChecker(pmps: Seq[PMP])(implicit p: Parameters) {
    def apply(addr: UInt, pgLevel: UInt): Bool = {
        ((true.B, 0.U.asTypeOf(new PMP)) /: pmps) { case ((h, prev), pmp) =>
            (h && pmp.homogeneous(addr, pgLevel, prev), pmp)
        }._1
    }
}

class PMPChecker(maxSize: Int)(implicit p: Parameters) extends LumiaModule {
    val io = IO(new Bundle() {
        val prv = UInt(INPUT, width = PRV.PRV_SZ)
        val pmp = Vec(nPMPs, new PMP().asInput)
        val addr = UInt(INPUT, width = paddrBits)
        val size = UInt(INPUT, width = log2Ceil(maxSize + 1))
        val r = Bool(OUTPUT)
        val w = Bool(OUTPUT)
        val x = Bool(OUTPUT)
    })

    val default = if (io.pmp.isEmpty) true.B else io.prv > PRV.S
    val pmp0 = Wire(init = 0.U.asTypeOf(new PMP))
    pmp0.cfg.r := default
    pmp0.cfg.w := default
    pmp0.cfg.x := default

    val res = (pmp0 /: (io.pmp zip (pmp0 +: io.pmp)).reverse) { case (prev, (pmp, prevPMP)) => {
            val hit = pmp.hit(io.addr, io.size, maxSize, prevPMP)
            val ignore = default && !pmp.cfg.l
            val aligned = pmp.aligned(io.addr, io.size, maxSize, prevPMP)
            val cur = Wire(init = pmp)
            cur.cfg.r := aligned && (pmp.cfg.r || ignore)
            cur.cfg.w := aligned && (pmp.cfg.w || ignore)
            cur.cfg.x := aligned && (pmp.cfg.x || ignore)
            Mux(hit, cur, prev)
        }
    }

    io.r := res.cfg.r
    io.w := res.cfg.w
    io.x := res.cfg.x
}

