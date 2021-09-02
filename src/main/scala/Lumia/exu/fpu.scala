package Lumia.exu

import Chisel._
import freechips.rocketchip.rocket.DecodeLogic
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile
import freechips.rocketchip.util._
import Lumia.utils._
import Lumia.common._
import hardfloat._

object FPUConsts {
    val S = BitPat("b1")
    val D = BitPat("b0")
    val RM_SZ = 3
    val FLAGS_SZ = 5
}

class FPUDecoder(implicit p: Parameters) extends LumiaModule {
    val io = IO(new Bundle() {
        val uop = Input(Bits(uopBits.W))
        val sigs = Output(new tile.FPUCtrlSigs())
    })
    import FPUConsts._
    val default = List(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X)
    val s = Array(
        BitPat(uopFCLASS_S)     -> List(N,N,Y,N,N,N, N, S, S, N, Y, N, N, N, N, N),
        BitPat(uopFMV_S_X)      -> List(X,X,N,N,N,N, N, S, D, Y, N, N, N, N, N, N),
        BitPat(uopFMV_X_S)      -> List(N,N,Y,N,N,N, N, D, S, N, Y, N, N, N, N, N),
        BitPat(uopFCVT_S_X)     -> List(X,X,N,N,N,N, N, S, S, Y, N, N, N, N, N, Y),
        BitPat(uopFCVT_X_S)     -> List(X,X,Y,N,N,N, N, S, S, N, Y, N, N, N, N, Y),
        BitPat(uopFSGNJ_S)      -> List(X,X,Y,Y,N,N, N, S, S, N, N, Y, N, N, N, N),
        BitPat(uopFMIN_S)       -> List(X,X,Y,Y,N,N, N, S, S, N, N, Y, N, N, N, Y),
        BitPat(uopFMAX_S)       -> List(X,X,Y,Y,N,N, N, S, S, N, N, Y, N, N, N, Y),
        BitPat(uopFADD_S)       -> List(X,X,Y,Y,N,N, Y, S, S, N, N, N, Y, N, N, Y),
        BitPat(uopFSUB_S)       -> List(X,X,Y,Y,N,N, Y, S, S, N, N, N, Y, N, N, Y),
        BitPat(uopFMUL_S)       -> List(X,X,Y,Y,N,N, N, S, S, N, N, N, Y, N, N, Y),
        BitPat(uopFMADD_S)      -> List(X,X,Y,Y,Y,N, N, S, S, N, N, N, Y,N,  N, Y),
        BitPat(uopFMSUB_S)      -> List(X,X,Y,Y,Y,N, N, S, S, N, N, N, Y, N, N, Y),
        BitPat(uopFNMADD_S)     -> List(X,X,Y,Y,Y,N, N, S, S, N, N, N, Y, N, N, Y),
        BitPat(uopFNMSUB_S)     -> List(X,X,Y,Y,Y,N, N, S, S, N, N, N, Y, N, N, Y)
    )
    val d = Array(
        BitPat(uopFCLASS_D)     -> List(X,X,Y,N,N,N, N, D, D, N, Y, N, N, N, N, N),
        BitPat(uopFCVT_S_D) -> List(X,X,Y,N,N, N,X,D,S,N,N,Y, N,N,N,Y),
        BitPat(uopFCVT_D_S) -> List(X,X,Y,N,N, N,X,S,D,N,N,Y, N,N,N,Y),
        BitPat(uopFCVT_D_X) -> List(X,X,N,N,N, X,X,D,D,Y,N,N, N,N,N,Y),
        BitPat(uopFCVT_X_D) -> List(X,X,Y,N,N, N,X,D,D,N,Y,N, N,N,N,Y),
        BitPat(uopFSGNJ_D)  -> List(X,X,Y,Y,N, N,N,D,D,N,N,Y, N,N,N,N),
        BitPat(uopFMIN_D)       -> List(X,X,Y,Y,N, N,N,D,D,N,N,Y, N,N,N,Y),
        BitPat(uopFMAX_D)       -> List(X,X,Y,Y,N, N,N,D,D,N,N,Y, N,N,N,Y),
        BitPat(uopFADD_D)   -> List(X,X,Y,Y,N, N,Y,D,D,N,N,N, Y,N,N,Y),
        BitPat(uopFSUB_D)   -> List(X,X,Y,Y,N, N,Y,D,D,N,N,N, Y,N,N,Y),
        BitPat(uopFMUL_D)   -> List(X,X,Y,Y,N, N,N,D,D,N,N,N, Y,N,N,Y),
        BitPat(uopFMADD_D)  -> List(X,X,Y,Y,Y, N,N,D,D,N,N,N, Y,N,N,Y),
        BitPat(uopFMSUB_D)  -> List(X,X,Y,Y,Y, N,N,D,D,N,N,N, Y,N,N,Y),
        BitPat(uopFNMADD_D) -> List(X,X,Y,Y,Y, N,N,D,D,N,N,N, Y,N,N,Y),
        BitPat(uopFNMSUB_D) -> List(X,X,Y,Y,Y, N,N,D,D,N,N,N, Y,N,N,Y)
    )
    val insns = s ++ d

    val decoder = DecodeLogic(io.uop, default, insns)
    val sg = io.sigs
    val sigs = Seq(sg.ldst, sg.wen, sg.ren1, sg.ren2, sg.ren3, sg.swap12, sg.swap23, sg.singleIn, sg.singleOut, sg.fromint
        ,   sg.toint, sg.fma, sg.fastpipe, sg.div, sg.sqrt, sg.wflags)
    sigs zip decoder map { case (f, s) => f := s }
}

class FMADecoder extends Module {
    val io = IO(new Bundle() {
        val uop = Input(UInt(uopBits.W))
        val cmd = Output(UInt(2.W))
    })

    val default: List[BitPat] = List(BitPat("b??"))
    val table: Array[(BitPat, List[BitPat])] = Array (
        BitPat(uopFADD_S)   -> List(BitPat("b00")),
        BitPat(uopFSUB_S)   -> List(BitPat("b01")),
        BitPat(uopFMUL_S)   -> List(BitPat("b00")),
        BitPat(uopFMADD_S)  -> List(BitPat("b00")),
        BitPat(uopFMSUB_S)  -> List(BitPat("b01")),
        BitPat(uopFNMADD_S) -> List(BitPat("b11")),
        BitPat(uopFNMSUB_S) -> List(BitPat("b10")),
        BitPat(uopFADD_D)   -> List(BitPat("b00")),
        BitPat(uopFSUB_D)   -> List(BitPat("b01")),
        BitPat(uopFMUL_D)   -> List(BitPat("b00")),
        BitPat(uopFMADD_D)  -> List(BitPat("b00")),
        BitPat(uopFMSUB_D)  -> List(BitPat("b01")),
        BitPat(uopFNMADD_D) -> List(BitPat("b11")),
        BitPat(uopFNMSUB_D) -> List(BitPat("b10"))
    )
    val decoder = DecodeLogic(io.uop, default, table)
    val (cmd: UInt)::Nil = decoder
    io.cmd := cmd
}

/***
 * WARNNING!!!
 * This section rewrites some modules in freechips.rocketchip.tile
 * JUST to avoid converting LumiaTileParams into TileParams
 */

class LumiaFPInput(implicit p: Parameters) extends LumiaBundle with tile.HasFPUCtrlSigs {
    val fmt = Bits(1.W)
    val rm = Bits(tile.FPConstants.RM_SZ.W)
    val fmaCmd = Bits(width = 2.W)
    val typ = Bits(width = 2.W)
    val in1 = Bits(width = (fLen+1).W)
    val in2 = Bits(width = (fLen+1).W)
    val in3 = Bits(width = (fLen+1).W)
}

class LumiaFPResult(implicit p: Parameters) extends LumiaBundle {
    val data = Bits(width = (fLen + 1).W)
    val exc = Bits(width = tile.FPConstants.FLAGS_SZ.W)
}


class LumiaFPToInt(implicit p: Parameters) extends LumiaModule
  with tile.HasFPUParameters
  with ShouldBeRetimed
{
    class Output extends Bundle {
        val in = new LumiaFPInput
        val lt = Bool()
        val store = Bits(width = fLen.W)
        val toint = Bits(width = xLen.W)
        val exc = Bits(width = tile.FPConstants.FLAGS_SZ.W)
        override def cloneType = new Output().asInstanceOf[this.type]
    }

    val io = IO(new Bundle() {
        val in = Flipped(Valid(new LumiaFPInput))
        val out = Valid(new Output)
    })

    val in = io.in.bits

    val dcmp = Module(new CompareRecFN(maxExpWidth, maxSigWidth))
    dcmp.io.a := in.in1
    dcmp.io.b := in.in2
    dcmp.io.signaling := !in.rm(1)

    val tag = !in.singleOut
    val store = ieee(in.in1)
    val toInt = Wire(UInt())
    val intType = Wire(UInt())

    toInt := store
    intType := tag

    io.out.bits.store := (floatTypes.map(t => Fill(maxType.ieeeWidth / t.ieeeWidth, store(t.ieeeWidth - 1, 0))): Seq[UInt])(tag)
    io.out.bits.toint := ((0 until nIntTypes).map(i => toInt((minXLen << i) - 1, 0).sextTo(xLen)): Seq[UInt])(intType)
    io.out.bits.exc := 0.U

    when (in.rm(0)) {
        val classify_out = (floatTypes.map(t => t.classify(maxType.unsafeConvert(in.in1, t))): Seq[UInt])(tag)
        toInt := classify_out | (store >> minXLen << minXLen)
        intType := 0.U
    }

    when (in.wflags) { // feq/flt/fle, fcvt
        toInt := (~in.rm & Cat(dcmp.io.lt, dcmp.io.eq)).orR | (store >> minXLen << minXLen)
        io.out.bits.exc := dcmp.io.exceptionFlags
        intType := 0.U

        when (!in.ren2) { // fcvt
            val cvtType = in.typ.extract(log2Ceil(nIntTypes), 1)
            intType := cvtType

            val conv = Module(new RecFNToIN(maxExpWidth, maxSigWidth, xLen))
            conv.io.in := in.in1
            conv.io.roundingMode := in.rm
            conv.io.signedOut := ~in.typ(0)
            toInt := conv.io.out
            io.out.bits.exc := Cat(conv.io.intExceptionFlags(2, 1).orR, 0.U(3.W), conv.io.intExceptionFlags(0))

            for (i <- 0 until nIntTypes-1) {
                val w = minXLen << i
                when (cvtType === i.U) {
                    val narrow = Module(new RecFNToIN(maxExpWidth, maxSigWidth, w))
                    narrow.io.in := in.in1
                    narrow.io.roundingMode := in.rm
                    narrow.io.signedOut := ~in.typ(0)

                    val excSign = in.in1(maxExpWidth + maxSigWidth) && !maxType.isNaN(in.in1)
                    val excOut = Cat(conv.io.signedOut === excSign, Fill(w-1, !excSign))
                    val invalid = conv.io.intExceptionFlags(2) || narrow.io.intExceptionFlags(1)
                    when (invalid) { toInt := Cat(conv.io.out >> w, excOut) }
                    io.out.bits.exc := Cat(invalid, 0.U(3.W), !invalid && conv.io.intExceptionFlags(0))
                }
            }
        }
    }

    io.out.valid := io.in.valid
    io.out.bits.lt := dcmp.io.lt || (dcmp.io.a.asSInt < 0.S && dcmp.io.b.asSInt >= 0.S)
    io.out.bits.in := in
}

class LumiaFPToFP(val latency: Int)(implicit p: Parameters) extends LumiaModule
  with tile.HasFPUParameters
  with ShouldBeRetimed
{
    val io = IO(new Bundle() {
        val in = Flipped(Valid(new LumiaFPInput))
        val out = Valid(new LumiaFPResult)
        val lt = Input(Bool())
    })

    val in = Pipe(io.in)

    val signNum = Mux(in.bits.rm(1), in.bits.in1 ^ in.bits.in2, Mux(in.bits.rm(0), ~in.bits.in2, in.bits.in2))
    val fsgnj = Cat(signNum(fLen), in.bits.in1(fLen-1, 0))

    val fsgnjMux = Wire(new LumiaFPResult)
    fsgnjMux.exc := 0.U
    fsgnjMux.data := fsgnj

    when (in.bits.wflags) { // fmin/fmax
        val isnan1 = maxType.isNaN(in.bits.in1)
        val isnan2 = maxType.isNaN(in.bits.in2)
        val isInvalid = maxType.isSNaN(in.bits.in1) || maxType.isSNaN(in.bits.in2)
        val isNaNOut = isnan1 && isnan2
        val isLHS = isnan2 || in.bits.rm(0) =/= io.lt && !isnan1
        fsgnjMux.exc := isInvalid << 4
        fsgnjMux.data := Mux(isNaNOut, maxType.qNaN, Mux(isLHS, in.bits.in1, in.bits.in2))
    }

    val inTag = !in.bits.singleIn // TODO typeTag
    val outTag = !in.bits.singleOut // TODO typeTag
    val mux = Wire(new LumiaFPResult)
    mux := fsgnjMux
    for (t <- floatTypes.init) {
        when (outTag === typeTag(t).U) {
            mux.data := Cat(fsgnjMux.data >> t.recodedWidth, maxType.unsafeConvert(fsgnjMux.data, t))
        }
    }

    when (in.bits.wflags && !in.bits.ren2) { // fcvt
        if (floatTypes.size > 1) {
            // widening conversions simply canonicalize NaN operands
            val widened = Mux(maxType.isNaN(in.bits.in1), maxType.qNaN, in.bits.in1)
            fsgnjMux.data := widened
            fsgnjMux.exc := maxType.isSNaN(in.bits.in1) << 4

            // narrowing conversions require rounding (for RVQ, this could be
            // optimized to use a single variable-position rounding unit, rather
            // than two fixed-position ones)
            for (outType <- floatTypes.init) when (outTag === typeTag(outType).U && (typeTag(outType).U === 0.U || outTag < inTag)) {
                val narrower = Module(new RecFNToRecFN(maxType.exp, maxType.sig, outType.exp, outType.sig))
                narrower.io.in := in.bits.in1
                narrower.io.roundingMode := in.bits.rm
                narrower.io.detectTininess := consts.tininess_afterRounding
                val narrowed = sanitizeNaN(narrower.io.out, outType)
                mux.data := Cat(fsgnjMux.data >> narrowed.getWidth, narrowed)
                mux.exc := narrower.io.exceptionFlags
            }
        }
    }

    io.out <> Pipe(in.valid, mux, latency-1)
}

class LumiaMulAddRecFNPipe(latency: Int, expWidth: Int, sigWidth: Int) extends Module
{
    require(latency<=2)

    val io = IO(new Bundle() {
        val validin = Input(Bool())
        val op = Input(Bits(2.W))
        val a = Input(Bits((expWidth + sigWidth + 1).W))
        val b = Input(Bits((expWidth + sigWidth + 1).W))
        val c = Input(Bits((expWidth + sigWidth + 1).W))
        val roundingMode = Input(UInt(3.W))
        val detectTininess = Input(UInt(1.W))
        val out = Output(Bits((expWidth + sigWidth + 1).W))
        val exceptionFlags = Output(Bits(5.W))
        val validout = Output(Bool())
    })

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
    val mulAddRecFNToRaw_preMul =
    Module(new MulAddRecFNToRaw_preMul(expWidth, sigWidth))
    val mulAddRecFNToRaw_postMul =
        Module(new MulAddRecFNToRaw_postMul(expWidth, sigWidth))

    mulAddRecFNToRaw_preMul.io.op := io.op
    mulAddRecFNToRaw_preMul.io.a  := io.a
    mulAddRecFNToRaw_preMul.io.b  := io.b
    mulAddRecFNToRaw_preMul.io.c  := io.c

    val mulAddResult =
        (mulAddRecFNToRaw_preMul.io.mulAddA *
          mulAddRecFNToRaw_preMul.io.mulAddB) +&
          mulAddRecFNToRaw_preMul.io.mulAddC

    val valid_stage0 = Wire(Bool())
    val roundingMode_stage0 = Wire(UInt(3.W))
    val detectTininess_stage0 = Wire(UInt(1.W))

    val postmul_regs = if(latency>0) 1 else 0
    mulAddRecFNToRaw_postMul.io.fromPreMul   := Pipe(io.validin, mulAddRecFNToRaw_preMul.io.toPostMul, postmul_regs).bits
    mulAddRecFNToRaw_postMul.io.mulAddResult := Pipe(io.validin, mulAddResult, postmul_regs).bits
    mulAddRecFNToRaw_postMul.io.roundingMode := Pipe(io.validin, io.roundingMode, postmul_regs).bits
    roundingMode_stage0                      := Pipe(io.validin, io.roundingMode, postmul_regs).bits
    detectTininess_stage0                    := Pipe(io.validin, io.detectTininess, postmul_regs).bits
    valid_stage0                             := Pipe(io.validin, false.B, postmul_regs).valid

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
    val roundRawFNToRecFN = Module(new RoundRawFNToRecFN(expWidth, sigWidth, 0))

    val round_regs = if(latency==2) 1 else 0
    roundRawFNToRecFN.io.invalidExc         := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.invalidExc, round_regs).bits
    roundRawFNToRecFN.io.in                 := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.rawOut, round_regs).bits
    roundRawFNToRecFN.io.roundingMode       := Pipe(valid_stage0, roundingMode_stage0, round_regs).bits
    roundRawFNToRecFN.io.detectTininess     := Pipe(valid_stage0, detectTininess_stage0, round_regs).bits
    io.validout                             := Pipe(valid_stage0, false.B, round_regs).valid

    roundRawFNToRecFN.io.infiniteExc := false.B

    io.out            := roundRawFNToRecFN.io.out
    io.exceptionFlags := roundRawFNToRecFN.io.exceptionFlags
}


class LumiaFPUFMAPipe(val latency: Int, val t: tile.FType)(implicit p: Parameters) extends LumiaModule
  with tile.HasFPUParameters
  with ShouldBeRetimed
{
    require(latency > 0)

    val io = IO(new Bundle() {
        val in = Flipped(Valid(new LumiaFPInput))
        val out = Valid(new LumiaFPResult)
    })

    val in = Reg(new LumiaFPInput)
    when (io.in.valid) {
        val one = 1.U << (t.sig + t.exp - 1)
        val zero = (io.in.bits.in1 ^ io.in.bits.in2) & (1.U << (t.sig + t.exp))
        val cmd_fma = io.in.bits.ren3
        val cmd_addsub = io.in.bits.swap23
        in := io.in.bits
        when (cmd_addsub) { in.in2 := one }
        when (!(cmd_fma || cmd_addsub)) { in.in3 := zero }
    }

    val fma = Module(new LumiaMulAddRecFNPipe((latency-1) min 2, t.exp, t.sig))
    fma.io.validin := io.in.valid
    fma.io.op := in.fmaCmd
    fma.io.roundingMode := in.rm
    fma.io.detectTininess := consts.tininess_afterRounding
    fma.io.a := in.in1
    fma.io.b := in.in2
    fma.io.c := in.in3

    val res = Wire(new LumiaFPResult)
    res.data := sanitizeNaN(fma.io.out, t)
    res.exc := fma.io.exceptionFlags

    io.out := Pipe(fma.io.validout, res, (latency-3) max 0)
}
//********************************************

class FPU(implicit p: Parameters) extends LumiaModule with tile.HasFPUParameters {
    val io = IO(new Bundle() {
        val req = Valid(new ExecUnitReq(65)).flip
        val resp = Valid(new ExecUnitResp(65))
        val fflags = Valid(new FFlagsResp)
    })

    val fp_decoder = Module(new FPUDecoder)
    fp_decoder.io.uop := io.req.bits.uop

    val fp_ctrl = fp_decoder.io.sigs
    val fp_rm = Mux(ImmGenRM(io.req.bits.ip) === 7.U, io.req.bits.fcsr_rm, ImmGenRM(io.req.bits.ip))

    def fuInput(minT: Option[tile.FType]): LumiaFPInput = {
        val req = Wire(new LumiaFPInput)
        val tag = fp_ctrl.singleIn
        req <> fp_ctrl

        req.rm := fp_rm
        req.in1 := unbox(io.req.bits.rs1_data, tag, minT)
        req.in2 := unbox(io.req.bits.rs2_data, tag, minT)
        req.in3 := unbox(io.req.bits.rs3_data, tag, minT)
        when (fp_ctrl.swap23) {
            req.in3 := req.in2
        }
        req.typ := ImmGenTyp(io.req.bits.ip)
        req.fmt := tag =/= Y
        when (io.req.bits.uop.uop === uopFMV_X_S) {
            req.fmt := 0.U
        }
        val fma_decoder = Module(new FMADecoder)
        fma_decoder.io.uop := io.req.bits.uop
        req.fmaCmd := fma_decoder.io.cmd
        req
    }

    val dfma = Module(new LumiaFPUFMAPipe(4, t = tile.FType.D))
    dfma.io.in.valid := io.req.valid && fp_ctrl.fma && !fp_ctrl.singleOut
    dfma.io.in.bits := fuInput(Some(dfma.t))

    val sfma = Module(new LumiaFPUFMAPipe(4, t = tile.FType.D))
    sfma.io.in.valid := io.req.valid && fp_ctrl.fma && fp_ctrl.singleOut
    sfma.io.in.bits := fuInput(Some(sfma.t))


    val fpiu = Module(new LumiaFPToInt)
    fpiu.io.in.valid := io.req.valid && (fp_ctrl.toint || (fp_ctrl.fastpipe && fp_ctrl.wflags))
    fpiu.io.in.bits := fuInput(None)


    val fpmu = Module(new LumiaFPToFP(2))
    fpmu.io.in.valid := io.req.valid && fp_ctrl.fastpipe
    fpmu.io.in.bits := fpiu.io.in.bits
    fpmu.io.lt := fpiu.io.out.bits.lt

    io.resp.valid := fpmu.io.out.valid || sfma.io.out.valid || dfma.io.out.valid || fpiu.io.out.valid
    io.resp.bits.data := Mux(dfma.io.out.valid, box(dfma.io.out.bits.data, true.B)
        ,   Mux(sfma.io.out.valid, box(sfma.io.out.bits.data, false.B)
            ,   Mux(fpiu.io.out.valid, fpiu.io.out.bits.toint ,box(fpmu.io.out.bits.data, !fp_ctrl.singleOut))))
    io.fflags.valid := io.resp.valid
    io.fflags.bits.flags := Mux(dfma.io.out.valid, dfma.io.out.bits.exc
        ,   Mux(sfma.io.out.valid, sfma.io.out.bits.exc
            ,   Mux(fpiu.io.out.valid, fpiu.io.out.bits.exc
                ,   fpmu.io.out.bits.exc)))
}