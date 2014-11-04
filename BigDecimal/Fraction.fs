module Fraction

open System
open BigDecimal

type Fraction( num : bigint, den : bigint ) =
    
    do
        if den = bigint 0 then
            raise( new DivideByZeroException( "Denominator cannot be 0" ) )

    member public this.Numerator   = num
    member public this.Denominator = den
    member public this.Decimal =
        if num <> 0I && den <> 0I then
            //TODO
            0M
        else
            0M
    
    member public this.Simplify( ) =
        let num = this.Numerator   / this.GCD( this.Numerator, this.Denominator )
        let den = this.Denominator / this.GCD( this.Numerator, this.Denominator )
        Fraction( num, den )

    //Get greatest common divisor of two numbers
    member private this.GCD( x : bigint, y : bigint ) =
        if y = 0I then x
        else this.GCD( y, ( x % y ) )

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
            raise( new DivideByZeroException( "Divisor cannot be 0" ) )
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
            raise( new DivideByZeroException( "Divisor cannot be 0" ) )
        //a / b/c = ac/b
        let num = scalar * self.Denominator
        let den = self.Numerator
        Fraction( num, den )

    //Negation operator
    static member ( ~- )( self : Fraction ) =
        Fraction( -self.Numerator, -self.Denominator )

    override this.ToString( ) =
        if this.Decimal < 0M then
            "-" + ( -this.Numerator ).ToString( ) + "/" + ( -this.Denominator ).ToString( )
        else
            this.Numerator.ToString( ) + "/" + this.Denominator.ToString( )

    override this.Equals( other ) =
        let other = other :?> Fraction
        ( this.Numerator = other.Numerator ) && ( this.Denominator = other.Denominator )

    override this.GetHashCode( ) =
        this.Decimal.GetHashCode( )

    new( ) = Fraction( bigint 1, bigint 1 )