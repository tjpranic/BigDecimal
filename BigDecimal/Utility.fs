namespace BigDecimal

module Utility =
    
    //Get greatest common divisor of two bigints
    let rec gcd( x : bigint, y : bigint ) =
        if y = 0I then
            x
        else
            gcd( y, ( x % y ) )

    //Factorial for bigints (does not accept negatives)
    let fac( n : bigint ) =
        match n with
        | _ when n = 0I -> n
        | _ when n = 1I -> n
        | _ -> [ 1I..n ] |> List.reduce( * )

    //pow for bigints (does not accept negatives)
    let pow( n : bigint, x : bigint ) =
        match x with
        | _ when x = 0I -> 1I
        | _ when x = 1I -> n
        | _ -> [ for i in 1I..x do yield n ] |> List.reduce( * )

    //Reverse a string
    let rev( s : string ) =
        new string( s.ToCharArray( ) |> Array.rev )

    //Return a certain amount of zeroes in string form
    let get_zeroes( amount : int ) =
        [ for i in 0..amount -> "0" ] |> List.reduce( + )

    //Split a string into certain sized groups
    let group_string( str : string, group_size : int ) =
        let rec group_loop( number : string, groups : string list ) =
            if number.Length > 0 then
                let group_and_number =
                    if group_size < number.Length then
                        ( number.Substring( 0, group_size ), number.Substring( group_size ) )
                    else
                        ( number, "" )
                let groups = fst( group_and_number ) :: groups
                group_loop( snd( group_and_number ), groups )
            else
                groups |> List.rev
        group_loop( str, [] )