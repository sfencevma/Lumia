package Lumia.exu

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile
import freechips.rocketchip.rocket.DecodeLogic
import hardfloat._
import Lumia.common._


class FDivDecoder(implicit p: Parameters) extends LumiaModule with tile.HasFPUParameters {
    val io = IO(new Bundle() {
        val uop = Input(UInt(uopBits.W))
        val sigs = Output(new tile.FPUCtrlSigs)
    })
    val default = List(X, X, X, X, X, X, X, X, X, X, X)
    val insns = Array (
        BitPat(uopFDIV_S)   -> List(X, X, Y, Y, X, X, X, X, Y, N, Y),
        BitPat(uopFDIV_D)   -> List(X, X, N, N, X, X, X, X, Y, N, Y),
        BitPat(uopFSQRT_S)  -> List(X, X, Y, Y, X, X, X, X, N, Y, Y),
        BitPat(uopFSQRT_D)  -> List(X, X, N, N, X, X, X, X, N, Y, Y)
    )

    val decoder = DecodeLogic(io.uop, default, insns)
    val s = io.sigs
    val sigs = Seq(s.swap12, s.swap23, s.singleIn, s.singleOut, s.fromint, s.toint, s.fma
        , s.fastpipe, s.div, s.sqrt, s.wflags)
    sigs zip decoder map { case (f, s) => f := s }
    s.wen := chisel3.DontCare
    s.ldst := chisel3.DontCare
    s.ren1 := chisel3.DontCare
    s.ren2 := chisel3.DontCare
    s.ren3 := chisel3.DontCare
}

class FpDivSqrt(implicit p: Parameters) extends ExecUnit(
    dataWidth = 65,
    writeFrf = true,
    hasFdiv = true,
    hasFcsr = true
) with tile.HasFPUParameters {

    val kill = io.req.bits.kill

    def upconvert(x: UInt) = {
        val s2d = Module(new RecFNToRecFN(inExpWidth = 8, inSigWidth = 24, outExpWidth = 11, outSigWidth = 53))
        s2d.io.in := x
        s2d.io.roundingMode := 0.U
        s2d.io.detectTininess := chisel3.DontCare
        s2d.io.out
    }
    //  Single to Double
    val in1_upconvert = upconvert(unbox(io.req.bits.rs1_data, true.B, Some(tile.FType.S)))
    val in2_upconvert = upconvert(unbox(io.req.bits.rs2_data, false.B, Some(tile.FType.S)))
    //
    val fdiv_decoder = Module(new FDivDecoder)
    fdiv_decoder.io.uop := io.req.bits.uop
    val tag = fdiv_decoder.io.sigs.singleIn
    val frs1 = Mux(tag === Y, in1_upconvert, unbox(io.req.bits.rs1_data, tag, Some(tile.FType.D)))
    val frs2 = Mux(tag === Y, in2_upconvert, unbox(io.req.bits.rs2_data, tag, Some(tile.FType.D)))
    //
    val divsqrt = Module(new DivSqrtRecF64)
    divsqrt.io.inValid := io.req.valid & !kill
    divsqrt.io.sqrtOp := fdiv_decoder.io.sigs.sqrt
    divsqrt.io.a := frs1
    divsqrt.io.b := Mux(divsqrt.io.sqrtOp, frs1, frs2)
    divsqrt.io.roundingMode := io.fcsr_rm
    divsqrt.io.detectTininess := chisel3.DontCare

    //  Double to single
    val downvert_d2s = Module(new RecFNToRecFN(inExpWidth = 11, inSigWidth = 53, outExpWidth = 8, outSigWidth = 24))
    downvert_d2s.io.in := sanitizeNaN(divsqrt.io.out, tile.FType.D)
    downvert_d2s.io.roundingMode := io.fcsr_rm
    downvert_d2s.io.detectTininess := chisel3.DontCare

    //  Response
    val wflags = divsqrt.io.exceptionFlags | Mux(fdiv_decoder.io.sigs.singleIn === S, downvert_d2s.io.exceptionFlags, 0.U)
    io.fresp.valid := divsqrt.io.outValid_div || divsqrt.io.outValid_sqrt
    io.fresp.bits.data := Mux(tag === Y, box(downvert_d2s.io.out, false.B), box(downvert_d2s.io.in, true.B))
    io.fflags.valid := (divsqrt.io.outValid_sqrt || divsqrt.io.outValid_div) & !kill
    io.fflags.bits.flags := wflags
    //
    io.fu_types.valid := divsqrt.io.inReady_div || divsqrt.io.inReady_sqrt
    io.fu_types.bits := FU_FPU
}