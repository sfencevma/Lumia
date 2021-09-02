package Lumia.utils

import Chisel._
import chisel3.experimental._
import Lumia.common._

object select_first {
    def apply(in: UInt, n: Int) = {
        val sels = Wire(Vec(n, UInt(in.getWidth.W)))
        var mask = in

        for (i <- 0 until n) {
            sels(i) := PriorityEncoderOH(mask)
            mask = mask & (~sels(i))
        }
        sels
    }
}

/**
 * PseudoLRU algorithm
 * @param numEntries Number of entries
 */

class PseudoLRU(numEntries: Int) {
    private val state_reg = RegInit(0.U((numEntries - 1).W))

    def get_next_state(state: UInt, way: UInt) = {
        var next_state = state << 1
        var idx = 1.U(1.W)
        for (i <- log2Up(numEntries) - 1 to 0 by -1) {
            val bit = way(i)
            next_state = next_state.asUInt().bitSet(idx, !bit)
            idx = Cat(idx, bit)
        }
        next_state(numEntries - 1, 1)
    }

    def get_replace_way(state: UInt) = {
        val shifted_state = state << 1
        var idx = 1.U(1.W)
        for (i <- log2Up(numEntries) - 1 to 0 by -1) {
            val in_bounds = Cat(idx, 1.U << i)(log2Up(numEntries) - 1, 0) < numEntries.U
            idx = Cat(idx, in_bounds && shifted_state(idx))
        }
        idx(log2Up(numEntries) - 1, 0)
    }

    def access(way: UInt): Unit = {
        state_reg := get_next_state(state_reg, way)
    }

    def replace = get_replace_way(state_reg)
}

//
class SimpleQueueIO[T <: Data](private val gen: T) extends Bundle {
    val enq = Flipped(Valid(gen))
    val deq = new Bundle() {val valid = Input(Bool()); val bits = Output(gen)}
    val full = Output(Bool())
    val empty = Output(Bool())
    val reset = Input(Bool())
}

class SimpleQueue[T <: Data](val gen: T,
                             val entries: Int)
                            (implicit compileOptions: chisel3.CompileOptions) extends Module {
    require(entries > 0, "InstQueue must have non-negative and non-zero of entries")
    val genType = if(compileOptions.declaredTypeMustBeUnbound) {
        requireIsChiselType(gen)
        gen
    } else {
        if(DataMirror.internal.isSynthesizable(gen)) {
            chisel3.chiselTypeOf(gen)
        } else {
            gen
        }
    }

    val io = IO(new SimpleQueueIO(gen))

    val ram         = Mem(entries, genType)
    val enq_ptr     = RegInit(0.U(log2Ceil(entries).W))
    val deq_ptr     = RegInit(0.U(log2Ceil(entries).W))
    val maybe_full  = RegInit(false.B)
    val empty       = enq_ptr === deq_ptr && !maybe_full
    val full        = enq_ptr === deq_ptr && maybe_full

    val do_enq      = io.enq.valid && !io.reset
    val do_deq      = io.deq.valid && !io.reset
    val do_reset    = io.reset

    when (do_reset) {
        enq_ptr := 0.U
        deq_ptr := 0.U
    } .otherwise {
        when (do_enq) {
            ram(enq_ptr) := io.enq.bits
            enq_ptr := enq_ptr + 1.U
        }
        when (do_deq) {
            deq_ptr := deq_ptr + 1.U
        }
    }
    when (do_enq =/= do_deq) {
        maybe_full := do_enq
    }
    io.deq.bits := ram(deq_ptr)

    io.full := full
    io.empty := empty
}

object rob_old {
    def apply(id_a: UInt, id_b: UInt) = {
        val id_a_older_than_id_b = Mux(id_a(id_a.getWidth - 1) ^ id_b(id_b.getWidth - 1)
            , id_a(id_a.getWidth - 2, 0) >= id_b(id_b.getWidth - 2, 0)
            , id_a(id_a.getWidth - 2, 0) < id_b(id_b.getWidth - 2, 0))
        id_a_older_than_id_b
    }
}

/**
 * Imm generate
 */
object ImmGen {
    private val LONGEST_IMM_SZ = 20
    def apply(ip: UInt, isel: UInt): SInt = {
        val sign = ip(LONGEST_IMM_SZ-1).asSInt
        val i30_20 = Mux(isel === IF_U, ip(18,8).asSInt, sign)
        val i19_12 = Mux(isel === IF_U || isel === IF_J, ip(7,0).asSInt, sign)
        val i11    = Mux(isel === IF_U, 0.S,
            Mux(isel === IF_J || isel === IF_B, ip(8).asSInt, sign))
        val i10_5  = Mux(isel === IF_U, 0.S, ip(18,14).asSInt)
        val i4_1   = Mux(isel === IF_U, 0.S, ip(13,9).asSInt)
        val i0     = Mux(isel === IF_S || isel === IF_I, ip(8).asSInt, 0.S)

        return Cat(sign, i30_20, i19_12, i11, i10_5, i4_1, i0).asSInt
    }
}

/**
 * ImmGenRM
 */
object ImmGenRM {
    def apply(ip: UInt): UInt = { return ip(2, 0) }
}

/**
 * ImmGenTyp
 */
object ImmGenTyp {
    def apply(ip: UInt): UInt = { return ip(9, 8) }
}

/**
 * Byte boundary
 */
object AlignPCToBoundary {
    def apply(pc: UInt, b: Int): UInt = {
        ~((~pc).asUInt() | (b - 1).U)
    }
}


/**
 * HoldUnless
 */
object HoldUnless {
    def apply[T <: Data](x: T, en: Bool): T = Mux(en, x, RegEnable(x, 0.U.asTypeOf(x), en))
}


/**
 * HoldControl
 */
object HoldControl {

    def apply(set: Bool, clr: Bool) = {
        val value = RegInit(false.B)
        val ena = set | clr
        val nxt = set | ~clr
        when (ena) {
            value := nxt
        }
        value
    }
}

/**
 * isOneOf
 */
object isOneOf {
    def apply[T <:Data](seq: Seq[T], e: T): Bool = {
        seq.map(_ == e).reduce(_|_).asBool()
    }
}


/**
 * Mask data
 */
object MaskData {
    def apply(oldData: UInt, newData: UInt, fullmask: UInt) = {
        (newData & fullmask) | (oldData & ~fullmask)
    }
}

