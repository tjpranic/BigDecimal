namespace BigDecimal

open System

module BigDecimal =
    
    type BigDecimal =
        static member MaxPrecision : int with get, set

        static member Zero : BigDecimal
        static member One  : BigDecimal

        static member ( + ) : BigDecimal * BigDecimal -> BigDecimal
        static member ( - ) : BigDecimal * BigDecimal -> BigDecimal
        static member ( * ) : BigDecimal * BigDecimal -> BigDecimal
        static member ( / ) : BigDecimal * BigDecimal -> BigDecimal

        static member Pow : BigDecimal * bigint -> BigDecimal
        static member Abs : BigDecimal -> BigDecimal

        static member ( ** ) : BigDecimal * bigint -> BigDecimal
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

    val pow      : BigDecimal * bigint -> BigDecimal
    val abs      : BigDecimal -> BigDecimal
    val nth_root : int * BigDecimal -> BigDecimal
    val sqrt     : BigDecimal -> BigDecimal
    val cbrt     : BigDecimal -> BigDecimal

    val is_decimal : BigDecimal -> bool
    val is_whole   : BigDecimal -> bool

    val to_bigint : BigDecimal -> bigint
    val floor     : BigDecimal -> BigDecimal
    val ceiling   : BigDecimal -> BigDecimal
    val round     : BigDecimal -> BigDecimal