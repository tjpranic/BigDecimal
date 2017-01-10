namespace Math

module BigDecimal =
    
    open System
    open System.Numerics
    
    type BigDecimal =
        static member MaxPrecision : int with get, set
        
        static member Zero : BigDecimal
        static member One  : BigDecimal
        
        static member (  + ) : BigDecimal * BigDecimal -> BigDecimal
        static member (  - ) : BigDecimal * BigDecimal -> BigDecimal
        static member (  * ) : BigDecimal * BigDecimal -> BigDecimal
        static member (  / ) : BigDecimal * BigDecimal -> BigDecimal
        static member ( ** ) : BigDecimal * BigInteger -> BigDecimal
        static member ( ~- ) : BigDecimal -> BigDecimal
        
        static member Pow : BigDecimal * BigInteger -> BigDecimal
        static member Abs : BigDecimal -> BigDecimal
        
        static member power        : BigDecimal -> BigInteger -> BigDecimal
        static member nthRoot      : BigDecimal -> int32      -> BigDecimal
        static member squareRoot   : BigDecimal -> BigDecimal
        static member cubeRoot     : BigDecimal -> BigDecimal
        static member abs          : BigDecimal -> BigDecimal
        static member floor        : BigDecimal -> BigDecimal
        static member ceiling      : BigDecimal -> BigDecimal
        static member round        : BigDecimal -> BigDecimal
        static member isDecimal    : BigDecimal -> bool
        static member isWhole      : BigDecimal -> bool
        static member toBigInteger : BigDecimal -> BigInteger

        static member op_Equality           : BigDecimal * BigDecimal -> bool
        static member op_Inequality         : BigDecimal * BigDecimal -> bool
        static member op_LessThan           : BigDecimal * BigDecimal -> bool
        static member op_LessThanOrEqual    : BigDecimal * BigDecimal -> bool
        static member op_GreaterThan        : BigDecimal * BigDecimal -> bool
        static member op_GreaterThanOrEqual : BigDecimal * BigDecimal -> bool
        
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