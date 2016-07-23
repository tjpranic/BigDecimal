namespace FSharpMath

module Fraction =
    
    open System
    open System.Numerics
    
    let rec gcd ( x : BigInteger ) ( y : BigInteger ) =
        if y = 0I then
            x
        else
            gcd y ( x % y )
            
    type Fraction( numerator : BigInteger, denominator : BigInteger ) =
        
        do
            if denominator = 0I then
                raise ( DivideByZeroException( "Denominator cannot be 0" ) )
                
        static member Zero = Fraction( 0I, 1I )
        static member One  = Fraction( 1I, 1I )
        
        member public this.Numerator   = numerator
        member public this.Denominator = denominator
        
        static member ( + )( self : Fraction, other : Fraction ) =
            //a/b + c/d = (a*d + c*b)/b*d
            let numerator   = ( self.Numerator * other.Denominator ) + ( other.Numerator * self.Denominator )
            let denominator = self.Denominator * other.Denominator
            Fraction( numerator, denominator )
            
        static member ( - )( self : Fraction, other : Fraction ) =
            //a/b - c/d = (a*d - c*b)/b*d
            let numerator   = ( self.Numerator * other.Denominator ) - ( other.Numerator * self.Denominator )
            let denominator = self.Denominator * other.Denominator
            Fraction( numerator, denominator )
            
        static member ( * )( self : Fraction, other : Fraction ) =
            //a/b * c/d = a*c/b*d
            let numerator   = self.Numerator * other.Numerator
            let denominator = self.Denominator * other.Denominator
            Fraction( numerator, denominator )
            
        static member ( / )( self : Fraction, other : Fraction ) =
            //a/b / c/d = a*d/b*c
            let numerator   = self.Numerator * other.Denominator
            let denominator = self.Denominator * other.Numerator
            Fraction( numerator, denominator )
            
        static member ( + )( self : Fraction, scalar : BigInteger ) =
            //b/c + a = (b + ac)/c
            let numerator   = self.Numerator + ( scalar * self.Denominator )
            let denominator = self.Denominator
            Fraction( numerator, denominator )
            
        static member ( - )( self : Fraction, scalar : BigInteger ) =
            //b/c - a = (b - ac)/c
            let numerator   = self.Numerator - ( scalar * self.Denominator )
            let denominator = self.Denominator
            Fraction( numerator, denominator )
            
        static member ( * )( self : Fraction, scalar : BigInteger ) =
            //b/c * a = ab/c
            let numerator   = scalar * self.Numerator
            let denominator = self.Denominator
            Fraction( numerator, denominator )
            
        static member ( / )( self : Fraction, scalar : BigInteger ) =
            if scalar = 0I then
                raise( DivideByZeroException( "Divisor cannot be 0" ) )
            //b/c / a = ac/b
            let numerator = scalar * self.Denominator
            let denominator = self.Numerator
            Fraction( denominator, numerator )
            
        static member ( + )( scalar : BigInteger, self : Fraction ) =
            //a + b/c = (ac + b)/c
            let numerator   = self.Numerator + ( scalar * self.Denominator )
            let denominator = self.Denominator
            Fraction( numerator, denominator )
            
        static member ( - )( scalar : BigInteger, self : Fraction ) =
            //a - b/c = (ac - b)/c
            let numerator   = ( scalar * self.Denominator ) - self.Numerator
            let denominator = self.Denominator
            Fraction( numerator, denominator )
            
        static member ( * )( scalar : BigInteger, self : Fraction ) =
            //a * b/c = ab/c
            let numerator   = scalar * self.Numerator
            let denominator = self.Denominator
            Fraction( numerator, denominator )
            
        static member ( / )( scalar : BigInteger, self : Fraction ) =
            if scalar = 0I then
                raise( DivideByZeroException( "Divisor cannot be 0" ) )
            //a / b/c = ac/b
            let numerator   = scalar * self.Denominator
            let denominator = self.Numerator
            Fraction( numerator, denominator )
            
        static member ( ~- )( self : Fraction ) =
            Fraction( -self.Numerator, -self.Denominator )
            
        member public this.simplify( ) =
            let numerator   = this.Numerator   / ( gcd this.Numerator this.Denominator )
            let denominator = this.Denominator / ( gcd this.Numerator this.Denominator )
            Fraction( numerator, denominator )
            
        override this.ToString( ) =
            string( this.Numerator ) + "/" + string( this.Denominator )
            
        override this.Equals( other ) =
            let other = other :?> Fraction //throws InvalidCastException on failure
            ( this.Numerator = other.Numerator ) && ( this.Denominator = other.Denominator )
            
        override this.GetHashCode( ) =
            ( this.Numerator.GetHashCode( ) * 17 ) + this.Denominator.GetHashCode( )
            
        new( ) = Fraction( 1I, 1I )