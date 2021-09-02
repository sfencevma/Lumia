package Lumia.utils

import Chisel._

case class ReadFn(fn: (Bool, Bool) => (Bool, Bool, UInt))
object ReadFn {
    def apply(x: (Bool, Bool) => (Bool, Bool, UInt)) = new ReadFn(x)
    def apply(x: Bool => (Bool, UInt)) =
        new ReadFn({case (_, oready) =>
            val (ovalid, data) = x(oready)
            (Bool(true), ovalid, data)
        })
    def apply(x: UInt): ReadFn = ReadFn(ready => (Bool(true), x))
    def apply(x: Unit): ReadFn = ReadFn(UInt(0))
}

case class WriteFn(fn: (Bool, Bool, UInt) => (Bool, Bool))
object WriteFn {
    def apply(x: (Bool, Bool, UInt) => (Bool, Bool)) = new WriteFn(x)
    def apply(x: (Bool, UInt) => Bool) =
        new WriteFn({ case (_, oready, data) =>
            (Bool(true), x(oready, data))
        })
    def apply(x: UInt)  : WriteFn = WriteFn((valid, data) => { when (valid) { x := data }; Bool(true)})
    def apply(x: Unit)  : WriteFn = WriteFn((valid, data) => { Bool(true) })
}

case class RegMapField(width: Int, readFn: ReadFn, writeFn: WriteFn) {
    require(width >= 0)
}

object RegMapField {
    type Map = Seq[(Int, Seq[RegMapField])]
    def apply(n: Int)                           : RegMapField = new RegMapField(n, ReadFn(()), WriteFn(()))
    def apply(n: Int, r: ReadFn, w: WriteFn)    : RegMapField = new RegMapField(n, r, w)
    def apply(n: Int, rw: UInt)                 : RegMapField = new RegMapField(n, ReadFn(rw), WriteFn(rw))
    def r(n: Int, r: ReadFn)                    : RegMapField = new RegMapField(n, r, WriteFn(()))
    def r(n: Int, r: UInt)                      : RegMapField = new RegMapField(n, ReadFn(r), WriteFn(()))
    def w(n: Int, w: WriteFn)                   : RegMapField = new RegMapField(n, ReadFn(()), w)
    def w(n: Int, w: UInt)                      : RegMapField = new RegMapField(n, ReadFn(()), WriteFn(w))
}