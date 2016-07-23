namespace FSharpMath

module BigDecimal =
    
    open System
    open System.Numerics
    
    type BigDecimal =
        static member MaxPrecision : int with get, set
        
        static member Zero : BigDecimal
        static member One  : BigDecimal
        
        static member ( + ) : BigDecimal * BigDecimal -> BigDecimal
        static member ( - ) : BigDecimal * BigDecimal -> BigDecimal
        static member ( * ) : BigDecimal * BigDecimal -> BigDecimal
        static member ( / ) : BigDecimal * BigDecimal -> BigDecimal
        
        static member Pow : BigDecimal * BigInteger -> BigDecimal
        static member Abs : BigDecimal -> BigDecimal
        
        static member ( ** ) : BigDecimal * BigInteger -> BigDecimal
        static member ( ~- ) : BigDecimal -> BigDecimal
        
        member public power    : BigInteger -> BigDecimal
        member public abs      : unit -> BigDecimal
        member public nth_root : int  -> BigDecimal
        member public sqrt     : unit -> BigDecimal
        member public cbrt     : unit -> BigDecimal
        
        member public is_decimal : unit -> bool
        member public is_whole   : unit -> bool
        
        member public to_biginteger : unit -> BigInteger
        
        member public floor   : unit -> BigDecimal
        member public ceiling : unit -> BigDecimal
        member public round   : unit -> BigDecimal
        
        interface IComparable
        
        override ToString    : unit -> String
        override Equals      : obj  -> bool
        override GetHashCode : unit -> int
        
        new : String     -> BigDecimal
        new : decimal    -> BigDecimal
        new : double     -> BigDecimal
        new : int32      -> BigDecimal
        new : int64      -> BigDecimal
        new : BigInteger -> BigDecimal
        new : unit       -> BigDecimal