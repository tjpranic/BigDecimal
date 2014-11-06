namespace BigDecimal

open System
open System.Numerics

module BigDecimal =
        
    type BigDecimal =
        static member get_Zero : unit -> BigDecimal

        static member ( + ) : BigDecimal * BigDecimal -> BigDecimal
        static member ( - ) : BigDecimal * BigDecimal -> BigDecimal
        static member ( * ) : BigDecimal * BigDecimal -> BigDecimal
        static member ( / ) : BigDecimal * BigDecimal -> BigDecimal

        static member ( ~- ) : BigDecimal -> BigDecimal

        member Pow  : BigDecimal -> BigDecimal
        member Sqrt : unit -> BigDecimal

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