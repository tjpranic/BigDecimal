namespace FSharpMath

module BigFraction =
    
    open System
    open System.Numerics
    
    type BigFraction =
        static member Zero : BigFraction
        static member One  : BigFraction
        
        member public Numerator   : BigInteger
        member public Denominator : BigInteger
        
        static member ( + ) : BigFraction * BigFraction -> BigFraction
        static member ( - ) : BigFraction * BigFraction -> BigFraction
        static member ( * ) : BigFraction * BigFraction -> BigFraction
        static member ( / ) : BigFraction * BigFraction -> BigFraction
        
        static member ( + ) : BigFraction * BigInteger -> BigFraction
        static member ( - ) : BigFraction * BigInteger -> BigFraction
        static member ( * ) : BigFraction * BigInteger -> BigFraction
        static member ( / ) : BigFraction * BigInteger -> BigFraction
        
        static member ( + ) : BigInteger * BigFraction -> BigFraction
        static member ( - ) : BigInteger * BigFraction -> BigFraction
        static member ( * ) : BigInteger * BigFraction -> BigFraction
        static member ( / ) : BigInteger * BigFraction -> BigFraction
        
        static member ( ~- ) : BigFraction -> BigFraction
        
        member public simplify : unit -> BigFraction
        
        override ToString    : unit -> String
        override Equals      : obj  -> bool
        override GetHashCode : unit -> int
        
        new : BigInteger * BigInteger -> BigFraction
        new : unit -> BigFraction