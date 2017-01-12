namespace System.Numerics

[<RequireQualifiedAccess>]
module BigInteger =
    
    let factorial ( n : BigInteger ) =
        match n with
        | _ when n = 0I -> 1I
        | _ when n = 1I -> n
        | _             -> [ 1I..n ] |> List.reduce ( * )
    
    let power ( n : BigInteger ) ( x : BigInteger ) =
        match x with
        | _ when x = 0I -> 1I
        | _ when x = 1I -> n
        | _             -> [ for i in 1I..x do yield n ] |> List.reduce ( * )
    