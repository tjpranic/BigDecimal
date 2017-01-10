namespace Math

module BigFraction =
    
    open System
    open System.Numerics
    open Math.BigDecimal
    
    type BigFraction( numerator : BigInteger, denominator : BigInteger ) =
        
        do
            if denominator = 0I then
                raise ( DivideByZeroException( "Denominator cannot be 0" ) )
                
        static member Zero = BigFraction( 0I, 1I )
        static member One  = BigFraction( 1I, 1I )
        
        member public this.Numerator   = numerator
        member public this.Denominator = denominator
        
        static member ( + )( self : BigFraction, other : BigFraction ) =
            let numerator   = ( self.Numerator * other.Denominator ) + ( other.Numerator * self.Denominator )
            let denominator = self.Denominator * other.Denominator
            BigFraction( numerator, denominator )
        
        static member ( - )( self : BigFraction, other : BigFraction ) =
            let numerator   = ( self.Numerator * other.Denominator ) - ( other.Numerator * self.Denominator )
            let denominator = self.Denominator * other.Denominator
            BigFraction( numerator, denominator )
        
        static member ( * )( self : BigFraction, other : BigFraction ) =
            let numerator   = self.Numerator * other.Numerator
            let denominator = self.Denominator * other.Denominator
            BigFraction( numerator, denominator )
        
        static member ( / )( self : BigFraction, other : BigFraction ) =
            let numerator   = self.Numerator * other.Denominator
            let denominator = self.Denominator * other.Numerator
            BigFraction( numerator, denominator )
        
        static member ( + )( self : BigFraction, scalar : BigInteger ) =
            let numerator   = self.Numerator + ( scalar * self.Denominator )
            let denominator = self.Denominator
            BigFraction( numerator, denominator )
        
        static member ( - )( self : BigFraction, scalar : BigInteger ) =
            let numerator   = self.Numerator - ( scalar * self.Denominator )
            let denominator = self.Denominator
            BigFraction( numerator, denominator )
        
        static member ( * )( self : BigFraction, scalar : BigInteger ) =
            let numerator   = scalar * self.Numerator
            let denominator = self.Denominator
            BigFraction( numerator, denominator )
        
        static member ( / )( self : BigFraction, scalar : BigInteger ) =
            if scalar = 0I then
                raise( DivideByZeroException( "Divisor cannot be 0" ) )
            let numerator = scalar * self.Denominator
            let denominator = self.Numerator
            BigFraction( denominator, numerator )
        
        static member ( + )( scalar : BigInteger, self : BigFraction ) =
            let numerator   = self.Numerator + ( scalar * self.Denominator )
            let denominator = self.Denominator
            BigFraction( numerator, denominator )
        
        static member ( - )( scalar : BigInteger, self : BigFraction ) =
            let numerator   = ( scalar * self.Denominator ) - self.Numerator
            let denominator = self.Denominator
            BigFraction( numerator, denominator )
        
        static member ( * )( scalar : BigInteger, self : BigFraction ) =
            let numerator   = scalar * self.Numerator
            let denominator = self.Denominator
            BigFraction( numerator, denominator )
        
        static member ( / )( scalar : BigInteger, self : BigFraction ) =
            if scalar = 0I then
                raise( DivideByZeroException( "Divisor cannot be 0" ) )
            let numerator   = scalar * self.Denominator
            let denominator = self.Numerator
            BigFraction( numerator, denominator )
        
        static member ( ~- )( self : BigFraction ) =
            BigFraction( -self.Numerator, -self.Denominator )
        
        static member gcd ( x : BigInteger ) ( y : BigInteger ) =
            if y = 0I then
                x
            else
                BigFraction.gcd y ( x % y )
        
        static member simplify ( n : BigFraction ) =
            let numerator   = n.Numerator   / ( BigFraction.gcd n.Numerator n.Denominator )
            let denominator = n.Denominator / ( BigFraction.gcd n.Numerator n.Denominator )
            BigFraction( numerator, denominator )
        
        static member toBigDecimal ( n : BigFraction ) =
            if n.Numerator <> 0I && n.Denominator <> 0I then
                BigDecimal( n.Numerator ) / BigDecimal( n.Denominator )
            else
                BigDecimal( 0 )
        
        static member op_Equality( self : BigFraction, other : BigFraction ) =
            let self  = BigFraction.simplify self
            let other = BigFraction.simplify other
            self.Numerator = other.Numerator && self.Denominator = other.Denominator
        
        static member op_Inequality( self : BigFraction, other : BigFraction ) =
            not ( self = other )
        
        static member op_LessThan( self : BigFraction, other : BigFraction ) =
            let self  = BigFraction.simplify self
            let other = BigFraction.simplify other
            if self.Denominator = other.Denominator then
                self.Numerator < other.Numerator
            else
                self.Denominator > other.Denominator
        
        static member op_LessThanOrEqual( self : BigFraction, other : BigFraction ) =
            let self  = BigFraction.simplify self
            let other = BigFraction.simplify other
            if self.Denominator = other.Denominator then
                self.Numerator >= other.Numerator
            else
                self.Denominator <= other.Denominator
        
        static member op_GreaterThan( self : BigFraction, other : BigFraction ) =
            let self  = BigFraction.simplify self
            let other = BigFraction.simplify other
            if self.Denominator = other.Denominator then
                self.Numerator > other.Numerator
            else
                self.Denominator < other.Denominator
        
        static member op_GreaterThanOrEqual( self : BigFraction, other : BigFraction ) =
            let self  = BigFraction.simplify self
            let other = BigFraction.simplify other
            if self.Denominator = other.Denominator then
                self.Numerator >= other.Numerator
            else
                self.Denominator <= other.Denominator
        
        interface IComparable with
            member this.CompareTo( obj ) =
                match obj with
                | null           -> 1
                | :? BigFraction ->
                    let other = ( obj :?> BigFraction )
                    match this with
                    | _ when this < other -> -1
                    | _ when this = other ->  0
                    | _ (*this > other*)  ->  1
                | _ -> raise <| InvalidOperationException( "Unable to compare object." )
        
        override this.ToString( ) =
            string( this.Numerator ) + "/" + string( this.Denominator )
        
        override this.Equals( other ) =
            let other = other :?> BigFraction //throws InvalidCastException on failure
            ( this.Numerator = other.Numerator ) && ( this.Denominator = other.Denominator )
        
        override this.GetHashCode( ) =
            ( this.Numerator.GetHashCode( ) * 17 ) + this.Denominator.GetHashCode( )
        
        new( ) = BigFraction( 1I, 1I )