namespace BigDecimal

open System

module BigDecimal =
    
    type BigDecimal =
        static member MaxPrecision : bigint

        static member Zero : BigDecimal
        static member One  : BigDecimal

        static member ( + ) : BigDecimal * BigDecimal -> BigDecimal
        static member ( - ) : BigDecimal * BigDecimal -> BigDecimal
        static member ( * ) : BigDecimal * BigDecimal -> BigDecimal
        static member ( / ) : BigDecimal * BigDecimal -> BigDecimal

        static member ( ~- ) : BigDecimal -> BigDecimal

        interface IComparable

        override ToString    : unit -> string
        override Equals      : obj  -> bool
        override GetHashCode : unit -> int

        new : string  -> BigDecimal
        new : decimal -> BigDecimal
        new : double  -> BigDecimal
        new : int32   -> BigDecimal
        new : int64   -> BigDecimal
        new : bigint  -> BigDecimal
        new : unit    -> BigDecimal
    
    type bigdec = BigDecimal

    val sqrt : BigDecimal -> BigDecimal
    val pow  : BigDecimal * BigDecimal -> BigDecimal