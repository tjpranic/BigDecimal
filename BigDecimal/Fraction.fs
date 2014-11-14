namespace BigDecimal

module Fraction =

    open BigDecimal.BigDecimal
    open BigDecimal.Utility

    open System

    type Fraction( numerator : bigint, denominator : bigint ) =

        do
            if denominator = 0I then
                raise( DivideByZeroException( "Denominator cannot be 0" ) )

        static member Zero = Fraction( 0I, 1I )
        static member One  = Fraction( 1I, 1I )

        member public this.Numerator   = numerator
        member public this.Denominator = denominator

        //Fraction and Fraction arithmetic
        static member ( + )( self : Fraction, other : Fraction ) =
            //a/b + c/d = (a*d + c*b)/b*d
            let num = ( self.Numerator * other.Denominator ) + ( other.Numerator * self.Denominator )
            let den = self.Denominator * other.Denominator
            Fraction( num, den )

        static member ( - )( self : Fraction, other : Fraction ) =
            //a/b - c/d = (a*d - c*b)/b*d
            let num = ( self.Numerator * other.Denominator ) - ( other.Numerator * self.Denominator )
            let den = self.Denominator * other.Denominator
            Fraction( num, den )

        static member ( * )( self : Fraction, other : Fraction ) =
            //a/b * c/d = a*c/b*d
            let num = self.Numerator * other.Numerator
            let den = self.Denominator * other.Denominator
            Fraction( num, den )

        static member ( / )( self : Fraction, other : Fraction ) =
            //a/b / c/d = a*d/b*c
            let num = self.Numerator * other.Denominator
            let den = self.Denominator * other.Numerator
            Fraction( num, den )
    
        //Fraction and Scalar arithmetic
        static member ( + )( self : Fraction, scalar : bigint ) =
            //a + b/c = (ac + b)/c
            let num = ( scalar * self.Denominator ) + self.Numerator
            let den = self.Denominator
            Fraction( num, den )

        static member ( - )( self : Fraction, scalar : bigint ) =
            //a - b/c = (b - ac)/c
            let num = self.Numerator - ( scalar * self.Denominator )
            let den = self.Denominator
            Fraction( num, den )

        static member ( * )( self : Fraction, scalar : bigint ) =
            //a * b/c = ab/c
            let num = scalar * self.Numerator
            let den = self.Denominator
            Fraction( num, den )

        static member ( / )( self : Fraction, scalar : bigint ) =
            if scalar = 0I then
                raise( DivideByZeroException( "Divisor cannot be 0" ) )
            //a / b/c = b/ac
            let num = scalar * self.Denominator
            let den = self.Numerator
            Fraction( den, num )

        static member ( + )( scalar : bigint, self : Fraction ) =
            //a + b/c = (ac + b)/c
            let num = ( scalar * self.Denominator ) + self.Numerator
            let den = self.Denominator
            Fraction( num, den )

        static member ( - )( scalar : bigint, self : Fraction ) =
            //a - b/c = (ac - b)/c
            let num = ( scalar * self.Denominator ) - self.Numerator
            let den = self.Denominator
            Fraction( num, den )

        static member ( * )( scalar : bigint, self : Fraction ) =
            //a * b/c = ab/c
            let num = scalar * self.Numerator
            let den = self.Denominator
            Fraction( num, den )

        static member ( / )( scalar : bigint, self : Fraction ) =
            if scalar = 0I then
                raise( DivideByZeroException( "Divisor cannot be 0" ) )
            //a / b/c = ac/b
            let num = scalar * self.Denominator
            let den = self.Numerator
            Fraction( num, den )

        //Negation operator
        static member ( ~- )( self : Fraction ) =
            Fraction( -self.Numerator, -self.Denominator )
        
        //Utility methods
        override this.ToString( ) =
            this.Numerator.ToString( ) + "/" + this.Denominator.ToString( )

        override this.Equals( other ) =
            let other = other :?> Fraction
            ( this.Numerator = other.Numerator ) && ( this.Denominator = other.Denominator )

        override this.GetHashCode( ) =
            ( this.Numerator.GetHashCode( ) * 17 ) + this.Denominator.GetHashCode( )

        new( ) = Fraction( 1I, 1I )

    let reduce( fraction : Fraction ) =
        let num = fraction.Numerator   / gcd( fraction.Numerator, fraction.Denominator )
        let den = fraction.Denominator / gcd( fraction.Numerator, fraction.Denominator )
        Fraction( num, den )

    let get_decimal( fraction : Fraction ) =
        if fraction.Numerator <> 0I && fraction.Denominator <> 0I then
            bigdec( fraction.Numerator ) / bigdec( fraction.Denominator )
        else
            bigdec( 0 )