namespace BigDecimal
    
module Fraction =
    
    open BigDecimal.BigDecimal

    type Fraction =
        static member Zero : Fraction
        static member One  : Fraction

        member public Numerator   : bigint
        member public Denominator : bigint

        static member ( + ) : Fraction * Fraction -> Fraction
        static member ( - ) : Fraction * Fraction -> Fraction
        static member ( * ) : Fraction * Fraction -> Fraction
        static member ( / ) : Fraction * Fraction -> Fraction

        static member ( + ) : Fraction * bigint -> Fraction
        static member ( - ) : Fraction * bigint -> Fraction
        static member ( * ) : Fraction * bigint -> Fraction
        static member ( / ) : Fraction * bigint -> Fraction

        static member ( + ) : bigint * Fraction -> Fraction
        static member ( - ) : bigint * Fraction -> Fraction
        static member ( * ) : bigint * Fraction -> Fraction
        static member ( / ) : bigint * Fraction -> Fraction

        static member ( ~- ) : Fraction -> Fraction

        override ToString    : unit -> string
        override Equals      : obj  -> bool
        override GetHashCode : unit -> int

        new : bigint * bigint -> Fraction
        new : unit -> Fraction
    
    val reduce      : Fraction -> Fraction
    val get_decimal : Fraction -> bigdec