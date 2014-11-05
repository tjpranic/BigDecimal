namespace BigDecimal
    
    module Fraction =
        
        open BigDecimal.BigDecimal

        type Fraction =
            
            member public Numerator   : bigint
            member public Denominator : bigint
            member public Decimal     : BigDecimal

            member public Simplify : unit -> Fraction

            static member ( + ) : Fraction * Fraction -> Fraction
            static member ( - ) : Fraction * Fraction -> Fraction
            static member ( * ) : Fraction * Fraction -> Fraction
            static member ( / ) : Fraction * Fraction -> Fraction

            static member ( ~- ) : Fraction -> Fraction

            override ToString    : unit -> string
            override Equals      : obj  -> bool
            override GetHashCode : unit -> int

            new : bigint * bigint -> Fraction
            new : unit -> Fraction
