namespace FSharpMath

module BigFraction =
    
    open System
    open System.Numerics
    
    let rec gcd ( x : BigInteger ) ( y : BigInteger ) =
        if y = 0I then
            x
        else
            gcd y ( x % y )
            
    type BigFraction( numerator : BigInteger, denominator : BigInteger ) =
        
        do
            if denominator = 0I then
                raise ( DivideByZeroException( "Denominator cannot be 0" ) )
                
        static member Zero = BigFraction( 0I, 1I )
        static member One  = BigFraction( 1I, 1I )
        
        member public this.Numerator   = numerator
        member public this.Denominator = denominator
        
        static member ( + )( self : BigFraction, other : BigFraction ) =
            //a/b + c/d = (a*d + c*b)/b*d
            let numerator   = ( self.Numerator * other.Denominator ) + ( other.Numerator * self.Denominator )
            let denominator = self.Denominator * other.Denominator
            BigFraction( numerator, denominator )
            
        static member ( - )( self : BigFraction, other : BigFraction ) =
            //a/b - c/d = (a*d - c*b)/b*d
            let numerator   = ( self.Numerator * other.Denominator ) - ( other.Numerator * self.Denominator )
            let denominator = self.Denominator * other.Denominator
            BigFraction( numerator, denominator )
            
        static member ( * )( self : BigFraction, other : BigFraction ) =
            //a/b * c/d = a*c/b*d
            let numerator   = self.Numerator * other.Numerator
            let denominator = self.Denominator * other.Denominator
            BigFraction( numerator, denominator )
            
        static member ( / )( self : BigFraction, other : BigFraction ) =
            //a/b / c/d = a*d/b*c
            let numerator   = self.Numerator * other.Denominator
            let denominator = self.Denominator * other.Numerator
            BigFraction( numerator, denominator )
            
        static member ( + )( self : BigFraction, scalar : BigInteger ) =
            //b/c + a = (b + ac)/c
            let numerator   = self.Numerator + ( scalar * self.Denominator )
            let denominator = self.Denominator
            BigFraction( numerator, denominator )
            
        static member ( - )( self : BigFraction, scalar : BigInteger ) =
            //b/c - a = (b - ac)/c
            let numerator   = self.Numerator - ( scalar * self.Denominator )
            let denominator = self.Denominator
            BigFraction( numerator, denominator )
            
        static member ( * )( self : BigFraction, scalar : BigInteger ) =
            //b/c * a = ab/c
            let numerator   = scalar * self.Numerator
            let denominator = self.Denominator
            BigFraction( numerator, denominator )
            
        static member ( / )( self : BigFraction, scalar : BigInteger ) =
            if scalar = 0I then
                raise( DivideByZeroException( "Divisor cannot be 0" ) )
            //b/c / a = ac/b
            let numerator = scalar * self.Denominator
            let denominator = self.Numerator
            BigFraction( denominator, numerator )
            
        static member ( + )( scalar : BigInteger, self : BigFraction ) =
            //a + b/c = (ac + b)/c
            let numerator   = self.Numerator + ( scalar * self.Denominator )
            let denominator = self.Denominator
            BigFraction( numerator, denominator )
            
        static member ( - )( scalar : BigInteger, self : BigFraction ) =
            //a - b/c = (ac - b)/c
            let numerator   = ( scalar * self.Denominator ) - self.Numerator
            let denominator = self.Denominator
            BigFraction( numerator, denominator )
            
        static member ( * )( scalar : BigInteger, self : BigFraction ) =
            //a * b/c = ab/c
            let numerator   = scalar * self.Numerator
            let denominator = self.Denominator
            BigFraction( numerator, denominator )
            
        static member ( / )( scalar : BigInteger, self : BigFraction ) =
            if scalar = 0I then
                raise( DivideByZeroException( "Divisor cannot be 0" ) )
            //a / b/c = ac/b
            let numerator   = scalar * self.Denominator
            let denominator = self.Numerator
            BigFraction( numerator, denominator )
            
        static member ( ~- )( self : BigFraction ) =
            BigFraction( -self.Numerator, -self.Denominator )
            
        member public this.simplify( ) =
            let numerator   = this.Numerator   / ( gcd this.Numerator this.Denominator )
            let denominator = this.Denominator / ( gcd this.Numerator this.Denominator )
            BigFraction( numerator, denominator )
            
        override this.ToString( ) =
            string( this.Numerator ) + "/" + string( this.Denominator )
            
        override this.Equals( other ) =
            let other = other :?> BigFraction //throws InvalidCastException on failure
            ( this.Numerator = other.Numerator ) && ( this.Denominator = other.Denominator )
            
        override this.GetHashCode( ) =
            ( this.Numerator.GetHashCode( ) * 17 ) + this.Denominator.GetHashCode( )
            
        new( ) = BigFraction( 1I, 1I )