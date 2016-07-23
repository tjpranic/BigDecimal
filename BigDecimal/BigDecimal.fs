namespace FSharpMath

module BigDecimal =
    
    open System
    open System.Numerics
    
    let rev ( s : string ) =
        new string( s.ToCharArray( ) |> Array.rev )
        
    let get_zeroes ( amount : int ) =
        [ for i in 0..amount -> "0" ] |> List.reduce ( + )
        
    let biginteger_power ( n : BigInteger ) ( x : BigInteger ) =
        match x with
        | _ when x = 0I -> 1I
        | _ when x = 1I -> n
        | _ -> [ for i in 1I..x do yield n ] |> List.reduce ( * )
        
    let group_string ( str : string ) ( group_size : int ) =
        let rec group_loop ( number : string ) ( groups : string list ) =
            if number.Length > 0 then
                let group_and_number =
                    if group_size < number.Length then
                        ( number.Substring( 0, group_size ), number.Substring( group_size ) )
                    else
                        ( number, "" )
                let groups = fst group_and_number :: groups
                group_loop ( snd group_and_number ) groups
            else
                groups |> List.rev
        group_loop str []
        
    let fac( n : BigInteger ) =
        match n with
        | _ when n = 0I -> n
        | _ when n = 1I -> n
        | _ -> [ 1I..n ] |> List.reduce ( * )
        
    let make_string ( integer : BigInteger ) ( scale : BigInteger ) =
        let s = string( integer )
        if scale > 0I then
            let decimal_pos = s.Length - int( scale )
            let result =
                if decimal_pos < 0 then
                    ( 0, get_zeroes ( ( abs decimal_pos ) - 1 ) + s ) //in case the number is supposed to have leading zeros
                else
                    ( decimal_pos, s )
                    
            let decimal_pos = fst result
            let s = snd result
            
            let decimal_notation = if decimal_pos = 0 then "0." else "."
            s.Insert( decimal_pos, decimal_notation )
        else
            s
            
    //this is up here because the Pow operator depends on the nth_root function
    let shifting_nth_root ( root : int ) ( integer : BigInteger ) ( scale : BigInteger ) ( precision : int ) =
        let number = make_string integer scale
        let group_number ( group_size : int ) ( number : string ) =
            let index      = number.IndexOf( '.' )
            let zeroes_req = index % root <> 0
            let number     = number.Remove( int( index ), 1 )
            
            //add leading zeros if necessary
            let prefix =
                if zeroes_req then
                    get_zeroes ( ( root - index % root ) - 1 )
                else
                    ""
            let number = prefix + number
            
            let group = group_string number root
            
            //add trailing zeros if necessary
            let last = group.[group.Length - 1]
            if last.Length <> root then
                let suffix = get_zeroes ( ( root - last.Length ) - 1 )
                group |> List.mapi ( fun i x -> if i = ( group.Length - 1 ) then x + suffix else x )
            else
                group
                
            //remove groups of all zeroes
            |> List.filter ( fun x -> BigInteger.Parse( x ) <> 0I )
            
        let rec nth_root ( groups : String list )
                         ( digits : String list )
                         ( x      : BigInteger )
                         ( y      : BigInteger )
                         ( r      : BigInteger )
                         ( alpha  : BigInteger )
                         ( beta   : int )
                         ( x2     : BigInteger )
                         ( y2     : BigInteger )
                         ( r2     : BigInteger )
                         ( count  : int ) =
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
                        |> List.filter ( fun beta -> ( ( ( numeric_base * y ) + BigInteger( beta ) ) ** root ) <= ( ( numeric_base ** root ) * x ) + alpha )
                        |> List.head
                        
                let x2 = ( ( numeric_base ** root ) * x ) + alpha
                let y2 = ( numeric_base * y ) + BigInteger( beta )
                let r2 = x2 - ( y2 ** root )
                
                let x, y, r = x2, y2, r2
                
                let digits = beta.ToString( ) :: digits
                
                nth_root groups digits x y r alpha beta x2 y2 r2 ( count + 1 )
            else
                digits |> List.rev
                
        let groups = group_number root number
        let digits = nth_root groups [] 0I 0I 0I 0I 0 0I 0I 0I 0
        
        let index = number.IndexOf( '.' )
        let posistion =
            if index <> -1 then
                if index % root <> 0 then
                    ( index / root ) + 1
                else
                    index / root
            else
                -1
                
        digits
            |> List.mapi ( fun i x -> if i = posistion then "." + x else x ) //add decimal point if necessary
            |> List.reduce ( + )
            
    type BigDecimal( number : string ) =
        
        //trim the number of unnecessary zeros
        let number =
            let trim_front ( s : string ) =
                let s =
                    let index = s.IndexOf( '.' )
                    if  index = -1 then s + ".0" else s
                    
                let pivot =
                    let temp = s.IndexOfAny( [| '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '.' |] )
                    if  temp = s.IndexOf( '.' ) then temp - 1 else temp
                    
                new string( Array.sub ( s.ToCharArray( ) ) pivot ( s.Length - pivot ) )
                
            let trim_back ( s : string ) =
                trim_front ( s |> rev )
                
            let trim( s : string ) =
                s |> trim_front |> trim_back |> rev
                
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
            let number_sans_point =
                let point_count = number.ToCharArray( ) |> Array.filter ( fun x -> x = '.' ) |> Array.length
                if point_count > 1 then
                    raise ( FormatException( "Multiple decimal points in input string" ) )
                    
                let index = number.IndexOf( '.' )
                if index <> -1 then
                    number.Remove( index, 1 )
                else
                    number
                    
            let result = number_sans_point |> BigInteger.TryParse
            
            if not ( fst result ) then
                raise ( FormatException( "Unable to parse number" ) )
                
            snd result
            
        static member ( + )( self : BigDecimal, other : BigDecimal ) =
            let larger_scale    = if self.Scale   > other.Scale   then self.Scale   else other.Scale
            let smaller_scale   = if self.Scale   < other.Scale   then self.Scale   else other.Scale
            let larger_integer  = if self.Integer > other.Integer then self.Integer else other.Integer
            let smaller_integer = if self.Integer < other.Integer then self.Integer else other.Integer
            
            //align the scales
            let readjusted_integer = smaller_integer * ( biginteger_power 10I ( larger_scale - smaller_scale ) )
            
            BigDecimal( make_string ( larger_integer + readjusted_integer ) larger_scale )
            
        static member ( - )( self : BigDecimal, other : BigDecimal ) =
            let larger_scale    = if self.Scale   > other.Scale   then self.Scale   else other.Scale
            let smaller_scale   = if self.Scale   < other.Scale   then self.Scale   else other.Scale
            let larger_integer  = if self.Integer > other.Integer then self.Integer else other.Integer
            let smaller_integer = if self.Integer < other.Integer then self.Integer else other.Integer
            
            let readjusted_integer = smaller_integer * ( biginteger_power 10I ( larger_scale - smaller_scale ) )
            
            let result = BigDecimal( make_string ( larger_integer - readjusted_integer ) larger_scale )
            
            if self.Integer < other.Integer then
                -result
            else
                result
                
        static member ( * )( self : BigDecimal, other : BigDecimal ) =
            BigDecimal( make_string ( self.Integer * other.Integer ) ( self.Scale + other.Scale ) )
            
        static member ( / )( self : BigDecimal, other : BigDecimal ) =
            //align the divisor and dividend scale if necessary
            let readjusted_divisor =
                if other.Scale < self.Scale then
                    other.Integer * ( biginteger_power 10I ( self.Scale - other.Scale ) )
                else
                    other.Integer
            let readjusted_dividend =
                if other.Scale > self.Scale then
                    self.Integer * ( biginteger_power 10I ( other.Scale - self.Scale ) )
                else
                    self.Integer
                    
            let result    = BigInteger.DivRem( readjusted_dividend, readjusted_divisor )
            let quotient  = fst result
            let remainder = snd result
            
            //DIVIDE
            if remainder > 0I then
                let divisor = readjusted_divisor
                let rec long_divide ( quotient : BigInteger ) ( remainder : BigInteger ) ( dividend : BigInteger ) ( decimals : string list ) =
                    if remainder <> 0I && BigInteger( decimals.Length ) <= BigInteger( BigDecimal.MaxPrecision ) then
                        let result    = BigInteger.DivRem( dividend, divisor )
                        let quotient  = fst( result )
                        let remainder = snd( result )
                        
                        let dividend =
                            if remainder < divisor then
                                remainder * 10I
                            else
                                dividend
                                
                        let decimals = string( quotient ) :: decimals
                        
                        long_divide quotient remainder dividend decimals
                    else
                        decimals
                            |> List.rev
                            |> List.mapi ( fun i x -> if i = 0 then x + "." else x ) //Add decimal point
                            |> List.reduce ( + )
                BigDecimal( long_divide quotient remainder readjusted_dividend [] )
            else
                BigDecimal( quotient )
                
        static member Pow( self : BigDecimal, power : BigInteger ) =
            let readjusted_self = self.Integer / 10I
            
            if power > 0I then
                BigDecimal( biginteger_power readjusted_self power )
            else
                BigDecimal.One / BigDecimal( biginteger_power readjusted_self ( abs power ) )
                
        static member Abs( self : BigDecimal ) =
            if self < BigDecimal.Zero then
                -self
            else
                self
                
        static member ( ** )( self : BigDecimal, power : BigInteger ) =
            BigDecimal.Pow( self, power )
            
        static member ( ~- )( self : BigDecimal ) =
            BigDecimal( make_string -self.Integer self.Scale )
            
        member this.power( power : BigInteger ) =
            BigDecimal.Pow( this, power )
            
        member public this.abs( ) =
            BigDecimal.Abs( this )
            
        member public this.nth_root( root : int ) =
            BigDecimal( shifting_nth_root root this.Integer this.Scale BigDecimal.MaxPrecision )
            
        member public this.sqrt( ) =
            this.nth_root( 2 )
            
        member public this.cbrt( ) =
            this.nth_root( 3 )
            
        member public this.is_decimal( ) =
            this.Scale > 1I
            
        member public this.is_whole( ) =
            not( this.is_decimal( ) )
            
        member public this.to_biginteger( ) =
            let string_rep = this.ToString( )
            let index = string_rep.IndexOf( '.' )
            if index <> 0 then
                BigInteger.Parse( string_rep.Substring( 0, string_rep.Length - index ) )
            else
                this.Integer
                
        member public this.floor( ) =
            BigDecimal( this.to_biginteger( ) )
            
        member public this.ceiling( ) =
            BigDecimal( this.to_biginteger( ) ) + BigDecimal.One
            
        member public this.round( ) =
            let string_rep = this.ToString( )
            let index = string_rep.IndexOf( '.' )
            if index <> 0 then
                let deteriminant = Int32.Parse( string_rep.[index + 1].ToString( ) )
                if deteriminant < 5 then
                    this.floor( )
                else
                    this.ceiling( )
            else
                this
                
        interface IComparable with
            member this.CompareTo( obj ) =
                let other = obj :?> BigDecimal //throws InvalidCastException on failure
                match this with
                | _ when this.Integer = other.Integer        -> -( this.Scale.CompareTo( other.Scale ) )
                | _ when this.Scale = other.Scale            ->  this.Integer.CompareTo( other.Integer )
                | _ when this.Scale = 1I || other.Scale = 1I ->  this.Integer.CompareTo( other.Integer ) //special case
                | _                                          -> -( this.Scale.CompareTo( other.Scale ) )
                
        override this.ToString( ) =
            make_string this.Integer this.Scale
            
        override this.Equals( obj ) =
            if obj = null then
                false
            else
                let other = obj :?> BigDecimal //throws InvalidCastException on failure
                this.Integer = other.Integer && this.Scale = other.Scale
                
        override this.GetHashCode( ) =
            ( this.Integer.GetHashCode( ) * 17 ) + this.Scale.GetHashCode( )
            
        new( num : decimal )    = BigDecimal( string( num ) )
        new( num : double )     = BigDecimal( string( num ) )
        new( num : int32 )      = BigDecimal( string( num ) )
        new( num : int64 )      = BigDecimal( string( num ) )
        new( num : BigInteger ) = BigDecimal( string( num ) )
        new( )                  = BigDecimal( "0" )