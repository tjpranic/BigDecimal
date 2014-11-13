namespace BigDecimal

module BigDecimal =
    
    open BigDecimal.BigString
    open BigDecimal.Utility

    open System
    open System.Numerics

    let make_string( integer : bigint, scale : bigint ) =
        let s = integer.ToString( )
        if scale <> 0I then
            if scale > bigint( Int32.MaxValue ) then
                "[Undisplayable]" //TODO: fix this
            else
                let decimal_pos = s.Length - int( scale )
                //In case the number is supposed to have leading zeros
                let result =
                    if decimal_pos < 0 then
                        let rec adjust_integer( decimal_pos : int, s : string ) =
                            if decimal_pos = 0 then
                                ( decimal_pos, s )
                            else
                                adjust_integer( decimal_pos + 1, "0" + s )
                        adjust_integer( decimal_pos, s )
                    else
                        ( decimal_pos, s )

                let decimal_pos = fst( result )
                let s = snd( result )    

                s.Insert( decimal_pos, match decimal_pos with
                                       | 0 -> "0."
                                       | _ -> "." )
        else
            s

    type BigDecimal( number : string ) =

        //Trim the number
        let number =
            let trim( s : string ) =
                let pivot = s.IndexOfAny( [| '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '.' |] )
                let s = new string( s.ToCharArray( )
                                        |> Array.mapi( fun i x -> if x = '0' && i < pivot && s.[i + 1] <> '.' then ' ' else x )
                                        |> Array.filter( fun x -> x <> ' ' ) )

                if s.IndexOf( '.' ) <> -1 then
                    let s = new string( s.ToCharArray( )
                                            |> Array.rev
                                            |> Array.mapi( fun i x -> if x = '0' && i <= pivot then ' ' else x )
                                            |> Array.filter( fun x -> x <> ' ' )
                                            |> Array.rev )
                    if s.[s.Length - 1] = '.' then s.Remove( s.IndexOf( '.' ) ) else s
                else
                    s
            trim( number )

        static member MaxPrecision = 50I

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
                if index > 0 then
                    number.Remove( index, 1 )
                else
                    number

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
                    if remainder <> 0I && bigint( decimals.Length ) <= BigDecimal.MaxPrecision then
                        let result    = BigInteger.DivRem( dividend, divisor )
                        let quotient  = fst( result )
                        let remainder = snd( result )

                        let dividend =
                            if remainder < divisor then
                                remainder * 10I
                            else
                                dividend
                    
                        let decimals = quotient.ToString( ) :: decimals

                        long_divide( quotient, remainder, dividend, decimals )
                    else
                        decimals
                            |> List.rev
                            |> List.mapi( fun i x -> if i = 0 then x + "." else x ) //Add decimal point
                            |> List.reduce( + )
                BigDecimal( long_divide( quotient, remainder, readjusted_dividend, [] ) )
            else
                BigDecimal( quotient )
    
        //Negation
        static member ( ~- )( self : BigDecimal ) =
            BigDecimal( make_string( -self.Integer, self.Scale ) : string )
        
        //Utility methods
        interface IComparable with
            member this.CompareTo( obj ) =
                if obj = null then
                    1
                else
                    let other = obj :?> BigDecimal //Throws InvalidCastException on failure
                    this.Integer.CompareTo( other.Integer ) //TODO: test this

        override this.ToString( ) =
            make_string( this.Integer, this.Scale )
    
        override this.Equals( obj ) =
            if obj = null then
                false
            else
                let other = obj :?> BigDecimal //Throws InvalidCastException on failure
                this.Integer = other.Integer && this.Scale = other.Scale

        override this.GetHashCode( ) =
            this.Integer.GetHashCode( )

        new( num : decimal ) = BigDecimal( num.ToString( ) )
        new( num : double  ) = BigDecimal( num.ToString( ) )
        new( num : int32   ) = BigDecimal( num.ToString( ) )
        new( num : int64   ) = BigDecimal( num.ToString( ) )
        new( num : bigint  ) = BigDecimal( num.ToString( ) )
        new( ) = BigDecimal( "0.0" )
    
    type bigdec = BigDecimal

    let sqrt( number : BigDecimal ) =
        let pairs =
            //Determines the order in which to pair
            //VERY important
            let order = number.Scale % 2I <> 0I
            let number =
                match order with
                | true  -> number.Integer.ToString( )
                | false -> rev( number.Integer.ToString( ) )

            let rec pair_number( number : string, pairs : String list ) =
                if number.Length = 0 then
                    pairs |> List.rev
                else
                    let pair =
                        match order with
                        | true  when number.Length = 1 -> number + "0"
                        | false when number.Length = 1 -> "0" + number
                        | true                         -> number.Substring( 0, 2 )
                        | false                        -> rev( number.Substring( 0, 2 ) )
                    let number =
                        match number.Length with
                        | 1 -> number.Remove( 0, 1 )
                        | _ -> number.Remove( 0, 2 )
                    let pairs = pair :: pairs
                    pair_number( number, pairs )
            let result = pair_number( number, [] ) |> List.filter( fun x -> x <> "00" )

            match order with
            | true  -> result
            | false -> result |> List.rev

        let guess_and_test( c : bigint, p : bigint ) =
            let rec loop( y : bigint, i : bigint ) =
                let y = i * ( ( 20I * p ) + i )
                if y <= c then
                    ( y, i )
                else
                    loop( y, i - 1I )
            loop( 0I, 9I )
            
        //SQUARE ROOT
        let rec sqrt( c : bigint, p : bigint, y : bigint, x : bigint, count : int, digits : String list ) =
            if bigint( count ) = BigDecimal.MaxPrecision then
                let decimal_pos =
                    let length = number.ToString( ).Length
                    let pos    = length - int( number.Scale )
                    match length with
                    | _ when length < 1 -> -1
                    | _ when pos % 2 <> 0 -> ( pos / 2 ) + 1
                    | _ -> pos / 2
                digits
                    |> List.rev
                    |> List.mapi( fun i x -> if i = decimal_pos then "." + x else x )
                    |> List.reduce( + )
            else
                let c =
                    if count < pairs.Length then
                        if count < 1 then
                            BigInteger.Parse( pairs.[count] )
                        else
                            BigInteger.Parse( ( c - y ).ToString( ) + pairs.[count] )
                    else
                        ( c - y ) * 100I

                let y, x   = guess_and_test( c, p )
                let digits = x.ToString( ) :: digits
                let p      = BigInteger.Parse( digits |> List.rev |> List.reduce( + ) )

                sqrt( c, p, y, x, count + 1, digits )
        BigDecimal( sqrt( 0I, 0I, 0I, 0I, 0, [] ) )

    let pow( number : BigDecimal, power : BigDecimal ) =
        if number.Scale = 0I && power.Scale = 0I then
            if power.Integer >= 0I then
                BigDecimal( pow( number.Integer, power.Integer ) )
            else
                ( new BigDecimal( "1.0" ) ) / ( new BigDecimal( pow( number.Integer, abs( power.Integer ) ) ) )
        else
            //TODO: implement this
            //convert to power to fraction then x ^ a/b = bth root of x ^ a
            BigDecimal( "0.0" )