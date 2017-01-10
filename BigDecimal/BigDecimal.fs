namespace Math

module BigDecimal =
    
    open System
    open System.Numerics
    open Math.BigInteger
    open Utility.String
    
    let makeString ( integer : BigInteger ) ( scale : BigInteger ) =
        let string = string( integer )
        if scale > 0I then
            let position, string =
                let position = string.Length - int( scale )
                if position < 0 then
                    ( 0, ( String.replicate ( abs position ) "0" ) + string ) //in case the number is supposed to have leading zeros
                else
                    ( position, string )
            let notation = if position = 0 then "0." else "."
            string.Insert( position, notation )
        else
            string
    
    type BigDecimal( number : string ) =
        
        //trim the number of unnecessary zeros, if they exist
        let number =
            let trimFront ( s : string ) =
                let s =
                    let index = s.IndexOf( '.' )
                    if  index = -1 then s + ".0" else s
                    
                let pivot =
                    let temp = s.IndexOfAny( [| '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '.' |] )
                    if  temp = s.IndexOf( '.' ) then temp - 1 else temp
                    
                new string( Array.sub ( s.ToCharArray( ) ) pivot ( s.Length - pivot ) )
            
            let trimBack ( s : string ) =
                trimFront ( s |> String.reverse )
            
            let trim( s : string ) =
                s |> trimFront |> trimBack |> String.reverse
            
            trim number
        
        static let mutable precision = 50
        static member MaxPrecision
            with get( )   = precision
            and  set( v ) = precision <- v
        
        static member Zero = BigDecimal( 0 )
        static member One  = BigDecimal( 1 )
        
        //Number of zeros after the decimal point
        member public this.Scale : BigInteger =
            BigInteger( number.Length - ( number.IndexOf( '.' ) + 1 ) )
        
        //number parsed as a BigInteger
        member public this.Integer : BigInteger =
            let numberSansPoint =
                let points = number.ToCharArray( ) |> Array.filter ( fun x -> x = '.' ) |> Array.length
                if points > 1 then
                    raise <| FormatException( "Multiple decimal points in input string" )
                
                let index = number.IndexOf( '.' )
                if index <> -1 then
                    number.Remove( index, 1 )
                else
                    number
                    
            let result = numberSansPoint |> BigInteger.TryParse
            
            if not ( fst result ) then
                raise <| FormatException( "Unable to parse number" )
            
            snd result
        
        static member ( + )( self : BigDecimal, other : BigDecimal ) =
            let largerScale    = if self.Scale   > other.Scale   then self.Scale   else other.Scale
            let smallerScale   = if self.Scale   < other.Scale   then self.Scale   else other.Scale
            let largerInteger  = if self.Integer > other.Integer then self.Integer else other.Integer
            let smallerInteger = if self.Integer < other.Integer then self.Integer else other.Integer
            
            //align the scales
            let adjustedInteger = smallerInteger * ( BigInteger.power 10I ( largerScale - smallerScale ) )
            
            BigDecimal( makeString ( largerInteger + adjustedInteger ) largerScale )
        
        static member ( - )( self : BigDecimal, other : BigDecimal ) =
            let largerScale    = if self.Scale   > other.Scale   then self.Scale   else other.Scale
            let smallerScale   = if self.Scale   < other.Scale   then self.Scale   else other.Scale
            let largerInteger  = if self.Integer > other.Integer then self.Integer else other.Integer
            let smallerInteger = if self.Integer < other.Integer then self.Integer else other.Integer
            
            let adjustedInteger = smallerInteger * ( BigInteger.power 10I ( largerScale - smallerScale ) )
            
            let result = BigDecimal( makeString ( largerInteger - adjustedInteger ) largerScale )
            if self.Integer < other.Integer then
                -result
            else
                result
        
        static member ( * )( self : BigDecimal, other : BigDecimal ) =
            BigDecimal( makeString ( self.Integer * other.Integer ) ( self.Scale + other.Scale ) )
        
        static member ( / )( self : BigDecimal, other : BigDecimal ) =
            //align the divisor and dividend scale if necessary
            let adjustedDivisor =
                if other.Scale < self.Scale then
                    other.Integer * ( BigInteger.power 10I ( self.Scale - other.Scale ) )
                else
                    other.Integer
            let adjustedDividend =
                if other.Scale > self.Scale then
                    self.Integer * ( BigInteger.power 10I ( other.Scale - self.Scale ) )
                else
                    self.Integer
            
            let quotient, remainder = BigInteger.DivRem( adjustedDividend, adjustedDivisor )
            
            //DIVIDE
            if remainder > 0I then
                let rec longDivide ( quotient : BigInteger ) ( remainder : BigInteger ) ( dividend : BigInteger ) ( decimals : string list ) =
                    if remainder = 0I || decimals.Length = BigDecimal.MaxPrecision then
                        decimals
                            |> List.rev
                            |> List.mapi ( fun i x -> if i = 0 then x + "." else x ) //add decimal point
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
            BigDecimal( makeString -self.Integer self.Scale )
        
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
        
        static member toBigInteger ( n : BigDecimal ) =
            let string = n.ToString( )
            let index  = string.IndexOf( '.' )
            if index <> 0 then
                BigInteger.Parse( string.Substring( 0, string.Length - index ) )
            else
                n.Integer
        
        static member power ( n : BigDecimal ) ( power : BigInteger ) =
            BigDecimal.Pow( n, power )
        
        static member nthRoot ( n : BigDecimal ) ( root : int32 ) =
            let radix  = 10I
            let number = n.ToString( )
            let rec extractRootDigits ( groups : String list )
                                      ( digits : String list )
                                      ( x      : BigInteger )
                                      ( y      : BigInteger )
                                      ( r      : BigInteger )
                                      ( alpha  : BigInteger )
                                      ( beta   : int )
                                      ( count  : int ) =
                if count = precision then
                    digits |> List.rev
                else
                    let alpha =
                        if count < groups.Length then
                            BigInteger.Parse( groups.[count] )
                        else
                            0I
                    
                    let beta =
                        [ for i in 0..9 -> i ]
                            |> List.rev
                            |> List.filter ( fun beta -> ( ( ( radix * y ) + BigInteger( beta ) ) ** root ) <= ( ( radix ** root ) * x ) + alpha )
                            |> List.head
                    
                    let x = ( ( radix ** root ) * x ) + alpha
                    let y = ( radix * y ) + BigInteger( beta )
                    let r = x - ( y ** root )
                    
                    let digits = beta.ToString( ) :: digits
                    
                    extractRootDigits groups digits x y r alpha beta ( count + 1 )
            
            let groups =
                let index  = number.IndexOf( '.' )
                let number = number.Remove( int( index ), 1 )
                
                //add leading zeros if necessary
                let prefix =
                    if index % root <> 0 then
                        String.replicate ( root - index % root ) "0"
                    else
                        ""
                let number = prefix + number
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
            
            let digits = extractRootDigits groups [] 0I 0I 0I 0I 0 0
            
            //determine where decimal point goes (if needed)
            let index = number.IndexOf( '.' )
            let position =
                if index <> -1 then
                    if index % root <> 0 then
                        ( index / root ) + 1
                    else
                        index / root
                else
                    -1
            
            BigDecimal(
                digits |> List.mapi ( fun i x -> if i = position then "." + x else x ) //add decimal point (if needed)
                       |> List.reduce ( + )
            )
        
        static member squareRoot ( n : BigDecimal ) =
            BigDecimal.nthRoot n 2
        
        static member cubeRoot ( n : BigDecimal ) =
            BigDecimal.nthRoot n 3
        
        static member abs ( n : BigDecimal ) =
            BigDecimal.Abs n
        
        static member floor ( n : BigDecimal ) =
            BigDecimal( BigDecimal.toBigInteger n )
        
        static member ceiling ( n : BigDecimal ) =
            BigDecimal( BigDecimal.toBigInteger n ) + BigDecimal.One
        
        static member round ( n : BigDecimal ) =
            let string = n.ToString( )
            let index  = string.IndexOf( '.' )
            if index <> 0 then
                let determinant = Int32.Parse( string.[index + 1].ToString( ) )
                if determinant < 5 then
                    BigDecimal.floor n
                else
                    BigDecimal.ceiling n
            else
                n
        
        static member isDecimal ( n : BigDecimal ) =
            n.Scale > 1I
        
        static member isWhole ( n : BigDecimal ) =
            not ( BigDecimal.isDecimal n )
        
        static member op_Equality( self : BigDecimal, other : BigDecimal ) =
            self.Integer = other.Integer && self.Scale = other.Scale
        
        static member op_Inequality( self : BigDecimal, other : BigDecimal ) =
            not ( self = other )
        
        static member op_LessThan( self : BigDecimal, other : BigDecimal ) =
            self.Integer < other.Integer || self.Scale < other.Scale
        
        static member op_LessThanOrEqual( self : BigDecimal, other : BigDecimal ) =
            self.Integer <= other.Integer || self.Scale <= other.Scale
        
        static member op_GreaterThan( self : BigDecimal, other : BigDecimal ) =
            self.Integer > other.Integer || self.Scale > other.Scale
        
        static member op_GreaterThanOrEqual( self : BigDecimal, other : BigDecimal ) =
            self.Integer >= other.Integer || self.Scale >= other.Scale
        
        interface IComparable with
            member this.CompareTo( obj ) =
                match obj with
                | null          -> 1
                | :? BigInteger -> this.Integer.CompareTo( obj :?> BigInteger )
                | :? BigDecimal ->
                    let other = ( obj :?> BigDecimal )
                    match this with
                    | _ when this < other -> -1
                    | _ when this = other ->  0
                    | _ (*this > other*)  ->  1
                | _ -> raise <| InvalidOperationException( "Unable to compare object." )
        
        override this.ToString( ) =
            makeString this.Integer this.Scale
        
        override this.Equals( obj ) =
            match obj with
            | null          -> false
            | :? BigDecimal -> this = ( obj :?> BigDecimal )
            | _             -> false
        
        override this.GetHashCode( ) =
            ( this.Integer.GetHashCode( ) * 17 ) + this.Scale.GetHashCode( )
        
        new( num : decimal    ) = BigDecimal( string( num ) )
        new( num : double     ) = BigDecimal( string( num ) )
        new( num : int32      ) = BigDecimal( string( num ) )
        new( num : int64      ) = BigDecimal( string( num ) )
        new( num : BigInteger ) = BigDecimal( string( num ) )
        new( )                  = BigDecimal( "0" )