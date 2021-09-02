package Lumia.common

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._

import Lumia.issue._

import scala.math.max

case object XLen extends Field[Int]

trait HasTileParameters {
    implicit val p: Parameters
    def tileParams: LumiaTileParams = p(LumiaTileKey)
    //  Features
    def usingVM: Boolean = tileParams.core.useVM
    def usingUser: Boolean = tileParams.core.useUser || usingVM
    def usingDebug: Boolean = tileParams.core.useDebug
    def usingSuperpage: Boolean = tileParams.core.useSuperpage

    def xLen: Int = p(XLen)
    def xBytes: Int = xLen / 8
    def iLen: Int = 32
    def hartId: Int = tileParams.hartId
    def hartIdLen: Int = p(MaxHartIdBits)
    def isaDTS: String = {
        val ie = if (tileParams.core.useRVE) "e" else "i"
        val m = if (tileParams.core.mulDiv.nonEmpty) "m" else ""
        val a = if (tileParams.core.useAtomics) "a" else ""
        val f = if (tileParams.core.fpu.nonEmpty) "f" else ""
        val d = if (tileParams.core.fpu.nonEmpty && tileParams.core.fpu.get.fLen > 32) "d" else ""
        val c = if (tileParams.core.useCompressed) "c" else ""
        val v = if (tileParams.core.useVector) "v" else ""
        s"rv${p(XLen)}$ie$m$a$f$d$c$v"
    }
    //  Virtual machine
    def usingBare: Boolean = tileParams.core.useBare
    def usingSv32: Boolean = tileParams.core.useSv32
    def usingSv39: Boolean = tileParams.core.useSv39
    def usingSv48: Boolean = tileParams.core.useSv48
    def vaddrBits: Int = if (usingVM) {
        if (usingBare) {
            16
        } else if (usingSv32) {
            32
        } else if (usingSv39) {
            39
        } else {
            48
        }
    } else {
        16
    }
    def paddrBits: Int = if (usingBare) {
        vaddrBits
    } else if (usingSv32) {
        34
    } else if (usingSv39) {
        56
    } else {
        56
    }
    def pgIdxBits: Int = 12
    def pgLevelBits: Int = 10 - log2Ceil(xLen / 32)
    def pgLevels: Int = p(PgLevels)
    def vpnBits: Int = vaddrBits - pgIdxBits
    def ppnBits: Int = paddrBits - pgIdxBits
    def asidBits: Int = p(ASIdBits)
    //  Mem bus width
    def memDataBits = 128
    def memDataBytes = memDataBits / 8
}

trait HasCoreParameters extends HasTileParameters {
    val coreParams: CoreParams = tileParams.core

    val fLen = coreParams.fpu.map(_.fLen).getOrElse(0)

    val usingMulDiv = coreParams.mulDiv.nonEmpty
    val usingFPU = coreParams.fpu.nonEmpty
    val usingAtomics = coreParams.useAtomics
    val usingAtomicOnlyForIO = coreParams.useAtomicsOnlyForIO
    val usingAtomicInCache = usingAtomics && !usingAtomicOnlyForIO
    val usingCompressed = coreParams.useCompressed
    val usingVector = coreParams.useVector
    val usingSCIE = coreParams.useSCIE

    val retireWidth = coreParams.retireWidth
    val fetchWidth = coreParams.fetchWidth
    val decodeWidth = coreParams.decodeWidth

    val fetchBytes = coreParams.fetchBytes
    val instBits = coreParams.instBits
    val instBytes = instBits / 8
}

/***
 *  Default Lumia core parameters
 */
case class LumiaCoreParams (
                              //*******************************
                              //  Common
                              bootFreqHz: BigInt = 0,
                              fetchWidth: Int = 1,
                              decodeWidth: Int = 1,
                              numRobEntries: Int = 64,
                              numStqEntries: Int = 32,
                              numPcqEntries: Int = 32,
                              numPaqEntries: Int = 32,
                              numIntPhysRegisters: Int = 128,
                              numFpPhysRegisters: Int = 64,
                              numIntCheckPoints: Int = 64,
                              numFpCheckPoints: Int = 64,
                              numIqEntries: Int = 16,
                              nL2TLBEntries: Int = 0,
                              mulDiv: Option[freechips.rocketchip.rocket.MulDivParams] = Some(MulDivParams(divEarlyOut = true)),
                              fpu: Option[freechips.rocketchip.tile.FPUParams] = Some(FPUParams()),
                              issueParams: Seq[IssueParams] = Seq (
                                  IssueParams(issueWidth = 1, numEntries = 16, lsu = false, float = false), //  Alu
                                  IssueParams(issueWidth = 1, numEntries = 16, lsu = false, float = false), //  BJP
                                  IssueParams(issueWidth = 2, numEntries = 16, lsu = false, float = false), //  Mul & Div
                                  IssueParams(issueWidth = 1, numEntries = 16, lsu = true , float = false),  //  Lsu
                                  IssueParams(issueWidth = 1, numEntries = 16, lsu = false, float = true ), //  FPU
                              ),
                              //*******************************
                              //  Predictor
                              globalHistoryLength: Int = 64,
                              numWeightTableEntries: Int = 128,
                              //*******************************
                              //  Function features
                              useCompressed: Boolean = true,
                              useFPU: Boolean = true,
                              useNMI: Boolean = false,
                              useAtomics: Boolean = true,
                              useAtomicsOnlyForIO: Boolean = false,
                              useDebug: Boolean = true,
                              useVM: Boolean = true,
                              useSuperpage: Boolean = false,
                              useUser: Boolean = true,
                              useSCIE: Boolean = false,
                              useRVE: Boolean = false,
                              useBPWatch: Boolean = false,
                              useBare: Boolean = false,
                              useSv32: Boolean = true,
                              useSv39: Boolean = false,
                              useSv48: Boolean = false,
                              //********************************
                              //  Misc
                              misaWritable: Boolean = true,
                              mtvecInit: Option[BigInt] = Some(BigInt(0)),
                              mtvecWritable: Boolean = true,
                              nPMPs: Int = 8,
                              nLocalInterrupts: Int = 0,
                              nBreakpoints: Int = 0,
                              nPerfCounters: Int = 0,
                              haveBasicCounters: Boolean = false,
                              haveCFlush: Boolean = false
                            ) extends freechips.rocketchip.tile.CoreParams {
    val haveFSDirty = useFPU
    val pmpGranularity: Int = 4
    val instBits: Int = 32
    val retireWidth = decodeWidth
    val maxCheckPoints = numIntCheckPoints max numFpCheckPoints
    override val useVector: Boolean = false
    override def lrscCycles: Int = 0
}

/**
 * Mixin trait to ad Lumia parameters to expand other traits/objects
 */
trait HasLumiaCoreParameters extends HasCoreParameters {
    val lumiaParams: LumiaCoreParams = tileParams.core

    //*******************************
    //  Core
    val bootAddr = "hfffffff0"
    val satpBits = 32
    val nPMPs = lumiaParams.nPMPs
    val pmpGranularity = lumiaParams.pmpGranularity
    //*******************************
    //  Superscalar widths
    val dispatchWidth = decodeWidth
    val issueWidth = 5
    val immBits = 32
    //*******************************
    //  Data structure sizes
    val numRobEntries = lumiaParams.numRobEntries
    val numStqEntries = lumiaParams.numStqEntries
    val numPcqEntries = lumiaParams.numPcqEntries
    val numPaqEntries = lumiaParams.numPaqEntries
    val numIqEntries  = lumiaParams.numIqEntries

    val numIntCheckPoints = lumiaParams.numIntCheckPoints
    val numFpCheckPoints = lumiaParams.numFpCheckPoints
    val gcTagSz = log2Ceil(numIntCheckPoints) max log2Ceil(numFpCheckPoints)

    val numIntPhysRegisters = lumiaParams.numIntPhysRegisters
    val numFpPhysRegisters = lumiaParams.numFpPhysRegisters

    val issueParams: Seq[IssueParams] = lumiaParams.issueParams

    //*******************************
    //  Predictor
    val globalHistoryLength = lumiaParams.globalHistoryLength
    val numWeightTableEntries = lumiaParams.numWeightTableEntries

    //*******************************
    //  Implicitly calculated constants
    val numLogicalRegisters = 32
    val lregSz          = log2Ceil(numLogicalRegisters)
    val ipregSz         = log2Ceil(numIntPhysRegisters)
    val fpregSz         = log2Ceil(numFpPhysRegisters)
    val pregSz          = ipregSz max fpregSz
    val iregBits        = 32
    val fregBits        = fLen
    val regBits         = iregBits max fregBits
    val robIdBits       = log2Ceil(numRobEntries) + 1
    val stqIdBits       = log2Ceil(numStqEntries) + 1
    val pcqIdBits       = log2Ceil(numPcqEntries) + 1
    val paqIdBits       = log2Ceil(numPaqEntries) + 1

    //*******************************
    //
    val mulDivParams = lumiaParams.mulDiv.getOrElse(MulDivParams())
    val fpuParams = lumiaParams.fpu.getOrElse(FPUParams())
    //  End
    //*******************************
}