﻿namespace BigDecimal

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
                    ( 0, ( [ for i in 0..abs( decimal_pos ) - 1 do yield "0" ] |> List.reduce( + ) ) + s )
                else
                    ( decimal_pos, s )

            let decimal_pos = fst( result )
            let s = snd( result )

            let decimal_notation = if decimal_pos = 0 then "0." else "."
            s.Insert( decimal_pos, decimal_notation )
        else
            s

    type BigDecimal( number : string ) =

        //Trim the number of unnecessary zeros
        let number =
            let trim( s : string ) =
                //Trim the front
                let pivot = s.IndexOfAny( [| '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '.' |] )
                let s = new string( s.ToCharArray( )
                                        |> Array.mapi( fun i x -> if x = '0' && i < pivot && s.[i + 1] <> '.' then ' ' else x )
                                        |> Array.filter( fun x -> x <> ' ' ) )

                if s.IndexOf( '.' ) <> -1 then
                    //Trim the back if necessary
                    let s = rev( s )
                    let pivot = s.IndexOfAny( [| '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '.' |] )
                    let s = new string( s.ToCharArray( )
                                            |> Array.mapi( fun i x -> if x = '0' && i <= pivot then ' ' else x )
                                            |> Array.filter( fun x -> x <> ' ' )
                                            |> Array.rev )
                    //Remove decimal point if necessary
                    if s.[s.Length - 1] = '.' then s.Remove( s.IndexOf( '.' ) ) else s
                else
                    s
            trim( number )
        
        //TODO: add ability to change decimal precision
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
                    if remainder <> 0I && bigint( decimals.Length ) <= BigDecimal.MaxPrecision then
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

    let sqrt( number : BigDecimal ) =
        let pairs =
            let rev_needed = number.Scale % 2I <> 0I

            let number_as_string =
                if rev_needed then
                    number.Integer.ToString( )
                else
                    rev( number.Integer.ToString( ) )

            let rec pair_number( number : string, pairs : string list ) =
                if number.Length = 0 then
                    pairs |> List.rev
                else if number.Length = 1 then
                    let pair   = number + "0"
                    let number = number.Remove( 0, 1 )
                    let pairs  = pair :: pairs
                    pairs |> List.rev
                else
                    let pair   = number.Substring( 0, 2 )
                    let number = number.Remove( 0, 2 )
                    let pairs  = pair :: pairs
                    pair_number( number, pairs )
            let result = pair_number( number_as_string, [] ) |> List.filter( fun x -> x <> "00" )
            
            if rev_needed then
                    result
                else
                    result |> List.rev |> List.map( fun x -> rev( x ) )

        let guess_and_test( c : bigint, p : bigint ) =
            let rec loop( y : bigint, i : bigint ) =
                let y = i * ( ( 20I * p ) + i )
                if y <= c then
                    ( y, i )
                else
                    loop( y, i - 1I )
            loop( 0I, 9I )
            
        //SQUARE ROOT
        let rec sqrt( c : bigint, p : bigint, y : bigint, x : bigint, count : int, digits : string list ) =
            if bigint( count ) = BigDecimal.MaxPrecision then
                //NOTE: There is a bug in here when sqrt( 1122.33445 )
                let decimal_pos =
                    let length = string( number ).Length
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
                            BigInteger.Parse( string( c - y ) + pairs.[count] )
                    else
                        ( c - y ) * 100I

                let y, x   = guess_and_test( c, p )
                let digits = string( x ) :: digits
                let p      = BigInteger.Parse( digits |> List.rev |> List.reduce( + ) )

                sqrt( c, p, y, x, count + 1, digits )
        BigDecimal( sqrt( 0I, 0I, 0I, 0I, 0, [] ) )

    let pow( number : BigDecimal, power : bigint ) =
        if power > 0I then
            BigDecimal( pow( number.Integer, power ) )
        else
            BigDecimal.One / BigDecimal( pow( number.Integer, abs( power ) ) )
               