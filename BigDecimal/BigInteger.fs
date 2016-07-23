namespace FSharpMath

module BigInteger =
    
    open System.Numerics
    
    type BigInteger with
        
        member this.factorial( ) =
            match this with
            | _ when this = 0I -> this
            | _ when this = 1I -> this
            | _ -> [ 1I..this ] |> List.reduce ( * )
        
        member this.power( x : BigInteger ) =
            match x with
            | _ when x = 0I -> 1I
            | _ when x = 1I -> this
            | _ -> [ for i in 1I..x do yield this ] |> List.reduce ( * )