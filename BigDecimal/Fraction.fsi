namespace FSharpMath

module Fraction =
    
    open System
    open System.Numerics
    
    type Fraction =
        static member Zero : Fraction
        static member One  : Fraction
        
        member public Numerator   : BigInteger
        member public Denominator : BigInteger
        
        static member ( + ) : Fraction * Fraction -> Fraction
        static member ( - ) : Fraction * Fraction -> Fraction
        static member ( * ) : Fraction * Fraction -> Fraction
        static member ( / ) : Fraction * Fraction -> Fraction
        
        static member ( + ) : Fraction * BigInteger -> Fraction
        static member ( - ) : Fraction * BigInteger -> Fraction
        static member ( * ) : Fraction * BigInteger -> Fraction
        static member ( / ) : Fraction * BigInteger -> Fraction
        
        static member ( + ) : BigInteger * Fraction -> Fraction
        static member ( - ) : BigInteger * Fraction -> Fraction
        static member ( * ) : BigInteger * Fraction -> Fraction
        static member ( / ) : BigInteger * Fraction -> Fraction
        
        static member ( ~- ) : Fraction -> Fraction
        
        member public simplify : unit -> Fraction
        
        override ToString    : unit -> String
        override Equals      : obj  -> bool
        override GetHashCode : unit -> int
        
        new : BigInteger * BigInteger -> Fraction
        new : unit -> Fraction