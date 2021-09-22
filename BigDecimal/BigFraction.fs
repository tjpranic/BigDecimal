namespace BigMath

open System
open System.Numerics

type BigFraction( numerator : BigInteger, denominator : BigInteger ) =

    do if denominator = 0I then raise <| DivideByZeroException( "Denominator cannot be 0." )

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
        let numerator   = self.Numerator   * other.Numerator
        let denominator = self.Denominator * other.Denominator
        BigFraction( numerator, denominator )

    static member ( / )( self : BigFraction, other : BigFraction ) =
        let numerator   = self.Numerator   * other.Denominator
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
        if scalar = 0I then raise <| DivideByZeroException( "Cannot divide by 0." )
        let numerator   = scalar * self.Denominator
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
        if scalar = 0I then raise <| DivideByZeroException( "Cannot divide by 0." )
        let numerator   = scalar * self.Denominator
        let denominator = self.Numerator
        BigFraction( numerator, denominator )

    static member ( ~- )( self : BigFraction ) =
        BigFraction( -self.Numerator, -self.Denominator )

    // These need to be static members because the comparison operators/method depend on them
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
        BigFraction.toBigDecimal( self ) < BigFraction.toBigDecimal( other )

    static member op_LessThanOrEqual( self : BigFraction, other : BigFraction ) =
        match self with
        | _ when self = other -> true
        | _                   -> self < other

    static member op_GreaterThan( self : BigFraction, other : BigFraction ) =
        match self with
        | _ when self = other -> false
        | _                   -> not ( self < other )

    static member op_GreaterThanOrEqual( self : BigFraction, other : BigFraction ) =
        match self with
        | _ when self = other -> true
        | _                   -> not ( self < other )

    interface IComparable with
        member this.CompareTo( obj ) =
            match obj with
            | null           -> 1
            | :? BigFraction ->
                let other = ( obj :?> BigFraction )
                match this with
                | _ when BigFraction.op_LessThan( this, other )   -> -1
                | _ when BigFraction.op_Equality( this, other )   ->  0
                | _ (*BigFraction.op_GreaterThan( this, other )*) ->  1
            | _ -> raise <| InvalidOperationException( "Unable to compare object." )

    override this.ToString( ) =
        string( this.Numerator ) + "/" + string( this.Denominator )

    override this.Equals( obj ) =
        match obj with
        | null           -> false
        | :? BigFraction -> BigFraction.op_Equality( this, ( obj :?> BigFraction ) )
        | _              -> false

    override this.GetHashCode( ) =
        ( this.Numerator.GetHashCode( ) * 17 ) + this.Denominator.GetHashCode( )

    new( numerator :  int32, denominator :  int32 ) = BigFraction( BigInteger( numerator ), BigInteger( denominator ) )
    new( numerator :  int64, denominator :  int64 ) = BigFraction( BigInteger( numerator ), BigInteger( denominator ) )
    new( numerator : uint32, denominator : uint32 ) = BigFraction( BigInteger( numerator ), BigInteger( denominator ) )
    new( numerator : uint64, denominator : uint64 ) = BigFraction( BigInteger( numerator ), BigInteger( denominator ) )
    new( )                                          = BigFraction( 1I, 1I )

[<RequireQualifiedAccess>]
[<CompilationRepresentation( CompilationRepresentationFlags.ModuleSuffix )>]
module BigFraction =

    let toBigDecimal ( n : BigFraction ) =
        if n.Numerator <> 0I && n.Denominator <> 0I then
            BigDecimal( n.Numerator ) / BigDecimal( n.Denominator )
        else
            BigDecimal( 0 )

    let gcd ( x : BigInteger ) ( y : BigInteger ) =
        BigFraction.gcd x y

    let simplify ( n : BigFraction ) =
        BigFraction.simplify n