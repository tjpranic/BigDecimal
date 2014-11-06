namespace BigDecimal

module Utility =
    
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

    //Get greatest common divisor of two numbers
    let rec gcd( x : bigint, y : bigint ) =
        if y = 0I then
            x
        else
            gcd( y, ( x % y ) )