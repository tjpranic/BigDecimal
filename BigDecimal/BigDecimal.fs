module BigDecimal

open System
open System.Numerics

let pow( n : bigint, x : bigint ) =
    match x with
    | _ when x = 0I -> 1I
    | _ when x = 1I -> n
    | _ -> [ for i in 1I..x do yield n ] |> List.reduce( * )

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
            if other.Integer < self.Integer then
                other.Integer * ( pow( 10I, self.Scale - other.Scale ) )
            else
                other.Integer

        let result    = BigInteger.DivRem( self.Integer, readjusted_divisor )
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
                        |> List.mapi( fun i x -> if i = 0 then x + "." else x )
                        |> List.reduce( + )
            BigDecimal( long_divide( quotient, remainder, self.Integer, [] ) )
        else
            BigDecimal( quotient )
    
    //Exponentiation
    member public this.Exp( power : BigDecimal ) =
        if this.Scale = 0I && power.Scale = 0I then
            if power.Integer >= 0I then
                BigDecimal( pow( this.Integer, power.Integer ) )
            else
                //TODO
                BigDecimal( "0.0" )
        else
            //TODO
            BigDecimal( "0.0" )

    //Negation
    static member ( ~- )( self : BigDecimal ) =
        BigDecimal( BigDecimal.MakeString( -self.Integer, self.Scale ) : string )

    //Utility methods
    override this.ToString( ) =
        BigDecimal.MakeString( this.Integer, this.Scale )
    
    static member public MaxPrecision : bigint = 15I

    static member private MakeString( integer : bigint, scale : bigint ) =
        let s = integer.ToString( )
        if scale <> 0I then
            if scale > bigint( Int32.MaxValue ) then
                "[Undisplayable]" //TODO: fix this
            else
                let decimal_pos = s.Length - int( scale )
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