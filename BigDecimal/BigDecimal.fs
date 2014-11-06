namespace BigDecimal

module BigDecimal =

    open BigDecimal.Utility

    open System
    open System.Numerics

    type BigDecimal( number : string ) =
        
        //Trim unnecessary zeros
        let number =
            let rec trim( number : string ) =
                if number.[0] = '0' && number.[1] <> '.' && number.Length > 1 then //trim leading zeros
                    trim( number.Remove( 0, 1 ) )
                else if number.[number.Length - 1] = '0' && number.Length > 1 then //trim trailing zeros
                    trim( number.Remove( number.Length - 1 ) )
                else if number.[number.Length - 1] = '.' then //remove decimal point if necessary
                    number.Remove( number.Length - 1, 1 )
                else
                    number

            if number.IndexOf( '.' ) = -1 then //If no decimal point
                let number = number + ".0"
                trim( number ) //Trim anyway to make sure no leading zeros go through
            else
                trim( number )
        
        static member private MaxPrecision : bigint = 15I
        static member get_Zero( ) = BigDecimal( 0 )

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
                    raise( new FormatException( "Multiple decimal points in input string" ) )

                let index = number.IndexOf( '.' )
                if index > 0 then
                    number.Remove( index, 1 )
                else
                    number

            let result = number_sans_point |> BigInteger.TryParse

            if not( fst( result ) ) then
                raise( new FormatException( "Unable to parse number" ) )

            snd( result )

        //BigDecimal and BigDecimal arithmetic
        static member ( + )( self : BigDecimal, other : BigDecimal ) =
            let larger_scale    = if self.Scale   > other.Scale   then self.Scale   else other.Scale
            let smaller_scale   = if self.Scale   < other.Scale   then self.Scale   else other.Scale
            let larger_integer  = if self.Integer > other.Integer then self.Integer else other.Integer
            let smaller_integer = if self.Integer < other.Integer then self.Integer else other.Integer

            //Align the scales
            let readjusted_integer = smaller_integer * ( pow( 10I, larger_scale - smaller_scale ) )

            BigDecimal( BigDecimal.MakeString( larger_integer + readjusted_integer, larger_scale ) : string )

        static member ( - )( self : BigDecimal, other : BigDecimal ) =
            let larger_scale    = if self.Scale   > other.Scale   then self.Scale   else other.Scale
            let smaller_scale   = if self.Scale   < other.Scale   then self.Scale   else other.Scale
            let larger_integer  = if self.Integer > other.Integer then self.Integer else other.Integer
            let smaller_integer = if self.Integer < other.Integer then self.Integer else other.Integer

            //Align the scales
            let readjusted_integer = smaller_integer * ( pow( 10I, larger_scale - smaller_scale ) )

            let result = BigDecimal( BigDecimal.MakeString( larger_integer - readjusted_integer, larger_scale ) : string )

            if self.Integer < other.Integer then
                -result
            else
                result

        static member ( * )( self : BigDecimal, other : BigDecimal ) =
            BigDecimal( BigDecimal.MakeString( self.Integer * other.Integer, self.Scale + other.Scale ) : string )

        static member ( / )( self : BigDecimal, other : BigDecimal ) =
            //Align the divisor scale if necessary
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
            BigDecimal( BigDecimal.MakeString( -self.Integer, self.Scale ) : string )
        
        //Exponentiation
        member public this.Pow( power : BigDecimal ) =
            if this.Scale = 0I && power.Scale = 0I then
                if power.Integer >= 0I then
                    BigDecimal( pow( this.Integer, power.Integer ) )
                else
                    ( new BigDecimal( "1.0" ) ) / ( new BigDecimal( pow( this.Integer, abs( power.Integer ) ) ) )
            else
                //TODO: implement this
                //convert to power to fraction then x ^ a/b = bth root of x ^ a
                BigDecimal( "0.0" )
        
        member public this.Sqrt( ) =
            //TODO: implement this
            BigDecimal( "0.0" )

        //Comparison
        interface IComparable with
            member this.CompareTo( obj ) =
                if obj = null then
                    1
                else
                    let other = obj :?> BigDecimal //Throws InvalidCastException on failure
                    this.Integer.CompareTo( other.Integer ) //TODO: test this

        //Utility methods
        override this.ToString( ) =
            BigDecimal.MakeString( this.Integer, this.Scale )
    
        override this.Equals( obj ) =
            if obj = null then
                false
            else
                let other = obj :?> BigDecimal //Throws InvalidCastException on failure
                this.Integer = other.Integer && this.Scale = other.Scale

        override this.GetHashCode( ) =
            this.Integer.GetHashCode( )

        static member private MakeString( integer : bigint, scale : bigint ) =
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

        new( num : decimal ) = BigDecimal( num.ToString( ) )
        new( num : double  ) = BigDecimal( num.ToString( ) )
        new( num : int32   ) = BigDecimal( num.ToString( ) )
        new( num : int64   ) = BigDecimal( num.ToString( ) )
        new( num : bigint  ) = BigDecimal( num.ToString( ) )
        new( ) = BigDecimal( "0.0" )