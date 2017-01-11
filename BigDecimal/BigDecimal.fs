namespace BigMath

open System
open System.Numerics

type BigDecimal( integer : BigInteger, scale : int32  ) =
    
    do if scale < 0 then raise <| InvalidOperationException( "Scale cannot be less than zero." )
    
    static let mutable precision = 50
    static member Precision
        with get( )       = precision
        and  set( value ) =
            if value < 1 then
                raise <| InvalidOperationException( "Precision cannot be less than 1." )
            else
                precision <- value
    
    static member Zero = BigDecimal( 0 )
    static member One  = BigDecimal( 1 )
    
    //digits of the number as a BigInteger
    member public this.Integer : BigInteger = integer
    //number of zeros after the decimal point
    member public this.Scale : int32 = scale
    
    static member ( + )( self : BigDecimal, other : BigDecimal ) =
        let largerScale    = if self.Scale   > other.Scale   then self.Scale   else other.Scale
        let smallerScale   = if self.Scale   < other.Scale   then self.Scale   else other.Scale
        let largerInteger  = if self.Integer > other.Integer then self.Integer else other.Integer
        let smallerInteger = if self.Integer < other.Integer then self.Integer else other.Integer
        
        //align the scales
        let adjustedInteger = smallerInteger * ( BigInteger.Pow ( 10I, ( largerScale - smallerScale ) ) )
        
        BigDecimal( ( largerInteger + adjustedInteger ), largerScale )
        
    static member ( - )( self : BigDecimal, other : BigDecimal ) =
        let largerScale    = if self.Scale   > other.Scale   then self.Scale   else other.Scale
        let smallerScale   = if self.Scale   < other.Scale   then self.Scale   else other.Scale
        let largerInteger  = if self.Integer > other.Integer then self.Integer else other.Integer
        let smallerInteger = if self.Integer < other.Integer then self.Integer else other.Integer
        
        let adjustedInteger = smallerInteger * ( BigInteger.Pow ( 10I, ( largerScale - smallerScale ) ) )
        
        let result = BigDecimal( ( largerInteger - adjustedInteger ), largerScale )
        if self.Integer < other.Integer then
            -result
        else
            result
        
    static member ( * )( self : BigDecimal, other : BigDecimal ) =
        BigDecimal( ( self.Integer * other.Integer ), ( self.Scale + other.Scale ) )
    
    static member ( / )( self : BigDecimal, other : BigDecimal ) =
        if other.Integer = 0I then raise <| DivideByZeroException( "Cannot divide by 0." )
        
        //align the divisor and dividend scale if necessary
        let adjustedDivisor =
            if other.Scale < self.Scale then
                other.Integer * ( BigInteger.Pow ( 10I, ( self.Scale - other.Scale ) ) )
            else
                other.Integer
        let adjustedDividend =
            if other.Scale > self.Scale then
                self.Integer * ( BigInteger.Pow ( 10I, ( other.Scale - self.Scale ) ) )
            else
                self.Integer
        
        let quotient, remainder = BigInteger.DivRem( adjustedDividend, adjustedDivisor )
        
        //DIVIDE
        if remainder > 0I then
            let rec longDivide ( quotient : BigInteger ) ( remainder : BigInteger ) ( dividend : BigInteger ) ( decimals : string list ) =
                if remainder = 0I || decimals.Length = BigDecimal.Precision then
                    decimals
                        |> List.rev
                        |> List.mapi   ( fun i x -> if i = 0 then x + "." else x ) //add decimal point
                        |> List.reduce ( + )
                else
                    let quotient, remainder = BigInteger.DivRem( dividend, adjustedDivisor )
                    let dividend =
                        if remainder < adjustedDivisor then
                            remainder * 10I
                        else
                            dividend
                    let decimals = string( quotient ) :: decimals
                    longDivide quotient remainder dividend decimals
            BigDecimal( longDivide quotient remainder adjustedDividend [] )
        else
            BigDecimal( quotient )
    
    static member ( ** )( self : BigDecimal, power : BigInteger ) =
        BigDecimal.Pow( self, power )
    
    static member ( ~- )( self : BigDecimal ) =
        BigDecimal( -self.Integer, self.Scale )
    
    static member Pow( self : BigDecimal, power : BigInteger ) =
        let adjustedSelf = self.Integer / 10I
        if power > 0I then
            BigDecimal( BigInteger.power adjustedSelf power )
        else
            BigDecimal.One / BigDecimal( BigInteger.power adjustedSelf ( abs power ) )
    
    static member Abs( self : BigDecimal ) =
        if self < BigDecimal.Zero then
            -self
        else
            self
    
    static member op_Equality( self : BigDecimal, other : BigDecimal ) =
        self.Integer = other.Integer && self.Scale = other.Scale
    
    static member op_Inequality( self : BigDecimal, other : BigDecimal ) =
        not ( self = other )
    
    static member op_LessThan( self : BigDecimal, other : BigDecimal ) =
        if self.Scale = other.Scale then
            self.Integer < other.Integer
        else
            self.Scale > other.Scale
    
    static member op_LessThanOrEqual( self : BigDecimal, other : BigDecimal ) =
        if self.Scale = other.Scale then
            self.Integer <= other.Integer
        else
            self.Scale >= other.Scale
    
    static member op_GreaterThan( self : BigDecimal, other : BigDecimal ) =
        if self.Scale = other.Scale then
            self.Integer > other.Integer
        else
            self.Scale < other.Scale
    
    static member op_GreaterThanOrEqual( self : BigDecimal, other : BigDecimal ) =
        if self.Scale = other.Scale then
            self.Integer >= other.Integer
        else
            self.Scale <= other.Scale
    
    interface IComparable with
        member this.CompareTo( obj ) =
            match obj with
            | null          -> 1
            | :? BigDecimal ->
                let other = ( obj :?> BigDecimal )
                match this with
                | _ when BigDecimal.op_LessThan( this, other )   -> -1
                | _ when BigDecimal.op_Equality( this, other )   ->  0
                | _ (*BigDecimal.op_GreaterThan( this, other )*) ->  1
            | _ -> raise <| InvalidOperationException( "Unable to compare object." )
    
    override this.ToString( ) =
        let string = string( this.Integer )
        if this.Scale > 0 then
            let position, string =
                let position = string.Length - this.Scale
                if position < 0 then
                    ( 0, ( String.replicate ( abs position ) "0" ) + string ) //in case the number is supposed to have leading zeros
                else
                    ( position, string )
            let notation = if position = 0 then "0." else "."
            string.Insert( position, notation )
        else
            string
    
    override this.Equals( obj ) =
        match obj with
        | null          -> false
        | :? BigDecimal -> BigDecimal.op_Equality( this, ( obj :?> BigDecimal ) )
        | _             -> false
    
    override this.GetHashCode( ) =
        ( this.Integer.GetHashCode( ) * 17 ) + this.Scale.GetHashCode( )
    
    new( n : string ) =
        if n = "" || n = null then raise <| ArgumentException( "String cannot be empty or null." )
        
        //trim the number of unnecessary zeros, if they exist
        let number =
            let number = String.trim n '0'
            match number.IndexOf( '.' ), number.Length with
            | -1, _ -> n
            |  0, 1 -> "0" + number + "0"
            |  0, _ -> "0" + number
            |  _, _ -> number
        
        let integer =
            let numberSansPoint =
                let points = number.ToCharArray( ) |> Array.filter ( fun x -> x = '.' ) |> Array.length
                if points > 1 then raise <| FormatException( "Multiple decimal points in input string." )
                
                let index = number.IndexOf( '.' )
                if index <> -1 then
                    number.Remove( index, 1 )
                else
                    number
            
            let result, integer = BigInteger.TryParse numberSansPoint
            if not result then raise <| FormatException( "Unable to parse number." )
            
            integer
        
        let scale =
            let index = number.IndexOf( '.' )
            if index <> -1 then
                number.Length - ( index + 1 )
            else
                0
        
        BigDecimal( integer, scale )
    
    new( n : decimal    ) = BigDecimal( string( n ) )
    new( n : double     ) = BigDecimal( string( n ) )
    new( n : int32      ) = BigDecimal( BigInteger( n ), 0 )
    new( n : int64      ) = BigDecimal( BigInteger( n ), 0 )
    new( n : uint32     ) = BigDecimal( BigInteger( n ), 0 )
    new( n : uint64     ) = BigDecimal( BigInteger( n ), 0 )
    new( n : BigInteger ) = BigDecimal( n,  0 )
    new( )                = BigDecimal( 0I, 0 )

[<RequireQualifiedAccess>]
[<CompilationRepresentation( CompilationRepresentationFlags.ModuleSuffix )>]
module BigDecimal =
    
    let toBigInteger ( n : BigDecimal ) =
        let string = n.ToString( )
        let index  = string.IndexOf( '.' )
        if index <> 0 then
            BigInteger.Parse( string.Substring( 0, string.Length - index ) )
        else
            n.Integer
    
    let power ( n : BigDecimal ) ( power : BigInteger ) =
        BigDecimal.Pow( n, power )
    
    let nthRoot ( n : BigDecimal ) ( root : int32 ) =
        let string =
            let string = n.ToString( )
            if n.Scale = 0 then
                string + ".0" //required to compute the position of the decimal point
            else
                string
        
        let groups =
            let index  = string.IndexOf( '.' )
            let string = string.Remove( index, 1 )
            
            //add leading zeros if necessary
            let prefix =
                if index % root <> 0 then
                    String.replicate ( root - index % root ) "0"
                else
                    ""
            
            let number = prefix + string
            let group  = String.group number root
            
            //add trailing zeros if necessary
            let last = group.[group.Length - 1]
            if last.Length <> root then
                let suffix = String.replicate ( root - last.Length ) "0"
                group |> List.mapi ( fun i x -> if i = ( group.Length - 1 ) then x + suffix else x )
            else
                group
            
            //remove groups of all zeroes
            |> List.filter ( fun x -> BigInteger.Parse( x ) <> 0I )
        
        let radix  = 10I
        let rec extractRootDigits ( digits : String list )
                                  ( x      : BigInteger  )
                                  ( y      : BigInteger  )
                                  ( r      : BigInteger  )
                                  ( alpha  : BigInteger  )
                                  ( beta   : BigInteger  )
                                  ( count  : int32       ) =
            if count = BigDecimal.Precision then
                digits |> List.rev
            else
                let alpha =
                    if count < groups.Length then
                        BigInteger.Parse( groups.[count] )
                    else
                        0I
                
                let beta =
                    BigInteger(
                        [ for i in 0..9 -> i ]
                            |> List.rev
                            |> List.filter ( fun beta -> ( ( ( radix * y ) + BigInteger( beta ) ) ** root ) <= ( ( radix ** root ) * x ) + alpha )
                            |> List.head )
                
                let x = ( ( radix ** root ) * x ) + alpha
                let y = ( radix * y ) + beta
                let r = x - ( y ** root )
                
                extractRootDigits ( beta.ToString( ) :: digits ) x y r alpha beta ( count + 1 )
        
        let digits = extractRootDigits [] 0I 0I 0I 0I 0I 0
        
        //determine where decimal point goes (if needed)
        let index = string.IndexOf( '.' )
        let position =
            if index <> -1 then
                if index % root <> 0 then
                    ( index / root ) + 1
                else
                    index / root
            else
                -1
        
        BigDecimal(
            digits |> List.mapi   ( fun i x -> if i = position then "." + x else x ) //add decimal point (if needed)
                   |> List.reduce ( + )
        )
    
    let squareRoot ( n : BigDecimal ) =
        nthRoot n 2
    
    let cubeRoot ( n : BigDecimal ) =
        nthRoot n 3
    
    let abs ( n : BigDecimal ) =
        BigDecimal.Abs n
    
    let floor ( n : BigDecimal ) =
        BigDecimal( toBigInteger n )
    
    let ceiling ( n : BigDecimal ) =
        BigDecimal( toBigInteger n ) + BigDecimal.One
    
    let round ( n : BigDecimal ) =
        let string = n.ToString( )
        let index  = string.IndexOf( '.' )
        if index <> 0 then
            let determinant = Int32.Parse( string.[index + 1].ToString( ) )
            if determinant < 5 then
                floor n
            else
                ceiling n
        else
            n
    
    let isDecimal ( n : BigDecimal ) =
        n.Scale > 1
    
    let isWhole ( n : BigDecimal ) =
        not ( isDecimal n )