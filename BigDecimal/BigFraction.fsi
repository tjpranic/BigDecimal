namespace Math

module BigFraction =
    
    open System
    open System.Numerics
    open Math.BigDecimal
    
    type BigFraction =
        
        static member Zero : BigFraction
        static member One  : BigFraction
        
        member public Numerator   : BigInteger
        member public Denominator : BigInteger
        
        static member (  + ) : BigFraction * BigFraction -> BigFraction
        static member (  - ) : BigFraction * BigFraction -> BigFraction
        static member (  * ) : BigFraction * BigFraction -> BigFraction
        static member (  / ) : BigFraction * BigFraction -> BigFraction
        static member (  + ) : BigFraction * BigInteger  -> BigFraction
        static member (  - ) : BigFraction * BigInteger  -> BigFraction
        static member (  * ) : BigFraction * BigInteger  -> BigFraction
        static member (  / ) : BigFraction * BigInteger  -> BigFraction
        static member (  + ) : BigInteger  * BigFraction -> BigFraction
        static member (  - ) : BigInteger  * BigFraction -> BigFraction
        static member (  * ) : BigInteger  * BigFraction -> BigFraction
        static member (  / ) : BigInteger  * BigFraction -> BigFraction
        static member ( ~- ) : BigFraction -> BigFraction
        
        static member gcd          : BigInteger  -> BigInteger -> BigInteger
        static member simplify     : BigFraction -> BigFraction
        static member toBigDecimal : BigFraction -> BigDecimal
        
        static member op_Equality           : BigFraction * BigFraction -> bool
        static member op_Inequality         : BigFraction * BigFraction -> bool
        static member op_LessThan           : BigFraction * BigFraction -> bool
        static member op_LessThanOrEqual    : BigFraction * BigFraction -> bool
        static member op_GreaterThan        : BigFraction * BigFraction -> bool
        static member op_GreaterThanOrEqual : BigFraction * BigFraction -> bool
        
        interface IComparable
        
        override ToString    : unit -> String
        override Equals      : obj  -> bool
        override GetHashCode : unit -> int
        
        new : BigInteger * BigInteger -> BigFraction
        new : unit -> BigFraction