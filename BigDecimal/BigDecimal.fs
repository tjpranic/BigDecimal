namespace BigDecimal

module BigDecimal =
    
    open BigDecimal.Utility

    open System
    open System.Numerics

    let make_string( integer : bigint, scale : bigint ) =
        let s = string( integer )
        if scale > 0I then
            let decimal_pos = s.Length - int( scale )
            let result =
                if decimal_pos < 0 then
                    //In case the number is supposed to have leading zeros
                    ( 0, get_zeroes( abs( decimal_pos ) - 1 ) + s )
                else
                    ( decimal_pos, s )

            let decimal_pos = fst( result )
            let s = snd( result )

            let decimal_notation = if decimal_pos = 0 then "0." else "."
            s.Insert( decimal_pos, decimal_notation )
        else
            s
    
    //This is up here because the Pow operator depends on the nth_root function
    let shifting_nth_root( root : int, integer : bigint, scale : bigint, precision : int ) =
        let number = make_string( integer, scale )
        let group_number( group_size : int, number : string ) =
            let index      = number.IndexOf( '.' )
            let zeroes_req = index % root <> 0
            let number     = number.Remove( int( index ), 1 )

            //Add leading zeros if necessary
            let prefix =
                if zeroes_req then
                    get_zeroes( ( root - index % root ) - 1 )
                else
                    ""
            let number = prefix + number

            let group = group_string( number, root )

            //Add trailing zeros if necessary
            let last = group.[group.Length - 1]
            if last.Length <> root then
                let suffix = get_zeroes( ( root - last.Length ) - 1 )
                group |> List.mapi( fun i x -> if i = ( group.Length - 1 ) then x + suffix else x )
            else
                group
            
            //Remove groups of all zeroes
            |> List.filter( fun x -> int( x ) <> 0 )

        let rec nth_root( groups : String list, digits : String list, x : bigint, y : bigint, r : bigint, alpha : bigint, beta : int, x2 : bigint, y2 : bigint, r2 : bigint, count : int ) =
            if count < precision then
                let alpha =
                    if count < groups.Length then
                        BigInteger.Parse( groups.[count] )
                    else
                        0I

                let numeric_base = 10I

                let beta =
                    [ for i in 0..9 -> i ]
                        |> List.rev
                        |> List.filter( fun beta -> ( ( ( numeric_base * y ) + bigint( beta ) ) ** root ) <= ( ( numeric_base ** root ) * x ) + alpha )
                        |> List.head

                let x2 = ( ( numeric_base ** root ) * x ) + alpha
                let y2 = ( numeric_base * y ) + bigint( beta )
                let r2 = x2 - ( y2 ** root )

                let x, y, r = x2, y2, r2

                let digits = beta.ToString( ) :: digits

                nth_root( groups, digits, x, y, r, alpha, beta, x2, y2, r2, count + 1 )
            else
                digits |> List.rev

        let groups = group_number( root, number )
        let digits = nth_root( groups, [], 0I, 0I, 0I, 0I, 0, 0I, 0I, 0I, 0 )

        let index = number.IndexOf( '.' )
        let pos =
            if index <> -1 then
                if index % root <> 0 then
                    ( index / root ) + 1
                else
                    ( index / root )
            else
                -1

        digits
            |> List.mapi( fun i x -> if i = pos then "." + x else x ) //Add decimal point if necessary
            |> List.reduce( + )

    type BigDecimal( number : string ) =
        
        //Trim the number of unnecessary zeros
        let number =
            let trim_front( s : string ) =
                let s =
                    let index = s.IndexOf( '.' )
                    if  index = -1 then s + ".0" else s

                let pivot =
                    let temp = s.IndexOfAny( [| '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '.' |] )
                    if  temp = s.IndexOf( '.' ) then temp - 1 else temp

                new string( Array.sub ( s.ToCharArray( ) ) pivot ( s.Length - pivot ) )

            let trim_back( s : string ) =
                trim_front( s |> rev )
    
            let trim( s : string ) =
                s |> trim_front |> trim_back |> rev

            trim( number )

        static let mutable precision = 50
        static member MaxPrecision
            with get( )   = precision
            and  set( v ) = precision <- v

        static member Zero = BigDecimal( 0 )
        static member One  = BigDecimal( 1 )

        //Number of zeros after the decimal point
        member public this.Scale : bigint =
            let index = number.IndexOf( '.' )
            if index = -1 then
                0I
            else
                bigint( number.Length - ( index + 1 ) )
    
        //number parsed as a bigint
        member public this.Integer : bigint =
            let number_sans_point =
                let point_count = number.ToCharArray( ) |> Array.filter( fun x -> x = '.' ) |> Array.length
                if point_count > 1 then
                    raise( FormatException( "Multiple decimal points in input string" ) )

                let index = number.IndexOf( '.' )
                if index = -1 then
                    number
                else
                    number.Remove( index, 1 )

            let result = number_sans_point |> BigInteger.TryParse

            if not( fst( result ) ) then
                raise( FormatException( "Unable to parse number" ) )

            snd( result )

        //BigDecimal and BigDecimal arithmetic
        static member ( + )( self : BigDecimal, other : BigDecimal ) =
            let larger_scale    = if self.Scale   > other.Scale   then self.Scale   else other.Scale
            let smaller_scale   = if self.Scale   < other.Scale   then self.Scale   else other.Scale
            let larger_integer  = if self.Integer > other.Integer then self.Integer else other.Integer
            let smaller_integer = if self.Integer < other.Integer then self.Integer else other.Integer

            //Align the scales
            let readjusted_integer = smaller_integer * ( pow( 10I, larger_scale - smaller_scale ) )

            BigDecimal( make_string( larger_integer + readjusted_integer, larger_scale ) : string )

        static member ( - )( self : BigDecimal, other : BigDecimal ) =
            let larger_scale    = if self.Scale   > other.Scale   then self.Scale   else other.Scale
            let smaller_scale   = if self.Scale   < other.Scale   then self.Scale   else other.Scale
            let larger_integer  = if self.Integer > other.Integer then self.Integer else other.Integer
            let smaller_integer = if self.Integer < other.Integer then self.Integer else other.Integer

            //Align the scales
            let readjusted_integer = smaller_integer * ( pow( 10I, larger_scale - smaller_scale ) )

            let result = BigDecimal( make_string( larger_integer - readjusted_integer, larger_scale ) : string )

            if self.Integer < other.Integer then
                -result
            else
                result

        static member ( * )( self : BigDecimal, other : BigDecimal ) =
            BigDecimal( make_string( self.Integer * other.Integer, self.Scale + other.Scale ) : string )

        static member ( / )( self : BigDecimal, other : BigDecimal ) =
            //Align the divisor and dividend scale if necessary
            let readjusted_divisor =
                if other.Scale < self.Scale then
                    other.Integer * ( pow( 10I, self.Scale - other.Scale ) )
                else
                    other.Integer
            let readjusted_dividend =
                if other.Scale > self.Scale then
                    self.Integer * ( pow( 10I, other.Scale - self.Scale ) )
                else
                    self.Integer

            let result    = BigInteger.DivRem( readjusted_dividend, readjusted_divisor )
            let quotient  = fst( result )
            let remainder = snd( result )

            //DIVIDE
            if remainder > 0I then
                let divisor = readjusted_divisor
                let rec long_divide( quotient : bigint, remainder : bigint, dividend : bigint, decimals : string list ) =
                    if remainder <> 0I && bigint( decimals.Length ) <= bigint( BigDecimal.MaxPrecision ) then
                        let result    = BigInteger.DivRem( dividend, divisor )
                        let quotient  = fst( result )
                        let remainder = snd( result )

                        let dividend =
                            if remainder < divisor then
                                remainder * 10I
                            else
                                dividend
                    
                        let decimals = string( quotient ) :: decimals

                        long_divide( quotient, remainder, dividend, decimals )
                    else
                        decimals
                            |> List.rev
                            |> List.mapi( fun i x -> if i = 0 then x + "." else x ) //Add decimal point
                            |> List.reduce( + )
                BigDecimal( long_divide( quotient, remainder, readjusted_dividend, [] ) )
            else
                BigDecimal( quotient )
       
        static member Pow( self : BigDecimal, power : BigDecimal ) =
            if self.Scale = 0I && power.Scale = 0I then
                if power.Integer > 0I then
                    BigDecimal( pow( self.Integer, power.Integer ) )
                else
                    BigDecimal.One / BigDecimal( pow( self.Integer, abs( power.Integer ) ) )
            else if self.Scale > 0I && power.Scale = 0I then
                
                BigDecimal.Zero
            else if self.Scale = 0I && power.Scale > 0I then
                
                BigDecimal.Zero
            else //self.Scale > 0I && power.Scale > 0I
                
                BigDecimal.Zero

        static member Abs( self : BigDecimal ) =
            if self < BigDecimal.Zero then
                -self
            else
                self

        static member ( ** )( self : BigDecimal, power : BigDecimal ) =
            BigDecimal.Pow( self, power )

        //Negation
        static member ( ~- )( self : BigDecimal ) =
            BigDecimal( make_string( -self.Integer, self.Scale ) : string )
        
        //Utility methods
        interface IComparable with
            member this.CompareTo( obj ) =
                let other = obj :?> BigDecimal //Throws InvalidCastException on failure
                match this with
                | _ when this.Integer = other.Integer        -> -( this.Scale.CompareTo( other.Scale ) )
                | _ when this.Scale = other.Scale            ->  this.Integer.CompareTo( other.Integer )
                | _ when this.Scale = 0I || other.Scale = 0I ->  this.Integer.CompareTo( other.Integer ) //Special case
                | _                                          -> -( this.Scale.CompareTo( other.Scale ) )
        
        override this.ToString( ) =
            make_string( this.Integer, this.Scale )
    
        override this.Equals( obj ) =
            if obj = null then
                false
            else
                let other = obj :?> BigDecimal //Throws InvalidCastException on failure
                this.Integer = other.Integer && this.Scale = other.Scale

        override this.GetHashCode( ) =
            ( this.Integer.GetHashCode( ) * 17 ) + this.Scale.GetHashCode( )

        new( num : decimal ) = BigDecimal( string( num ) )
        new( num : double  ) = BigDecimal( string( num ) )
        new( num : int32   ) = BigDecimal( string( num ) )
        new( num : int64   ) = BigDecimal( string( num ) )
        new( num : bigint  ) = BigDecimal( string( num ) )
        new( ) = BigDecimal( "0" )
    
    type bigdec = BigDecimal

    let pow( number : BigDecimal, power : BigDecimal ) =
        BigDecimal.Pow( number, power )

    let abs( number : BigDecimal ) =
        BigDecimal.Abs( number )

    let nth_root( root : int, number : BigDecimal ) =
        BigDecimal( shifting_nth_root( root, number.Integer, number.Scale, BigDecimal.MaxPrecision ) )

    let sqrt( number : BigDecimal ) =
        nth_root( 2, number )

    let cbrt( number : BigDecimal ) =
        nth_root( 3, number )

    let is_decimal( number : BigDecimal ) =
        number.Scale > 0I

    let is_whole( number : BigDecimal ) =
        not( is_decimal( number ) )

    let to_bigint( number : BigDecimal ) =
        let string_rep = number.ToString( )
        let index = string_rep.IndexOf( '.' )
        if index <> 0 then
            BigInteger.Parse( string_rep.Substring( 0, string_rep.Length - index ) )
        else
            number.Integer

    let floor( number : BigDecimal ) =
        BigDecimal( to_bigint( number ) )

    let ceiling( number : BigDecimal ) =
        BigDecimal( to_bigint( number ) ) + BigDecimal.One

    let round( number : BigDecimal ) =
        let string_rep = number.ToString( )
        let index = string_rep.IndexOf( '.' )
        if index <> 0 then
            let deteriminant = Int32.Parse( string_rep.[index + 1].ToString( ) )
            if deteriminant < 5 then
                floor( number )
            else
                ceiling( number )
        else
            number