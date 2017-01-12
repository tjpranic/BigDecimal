namespace System

[<RequireQualifiedAccess>]
module String =
    
    let rev ( s : string ) =
        new string( s.ToCharArray( ) |> Array.rev )
    
    let trimFront ( character : char ) ( s : string ) =
        s |> Seq.toArray
          |> Array.findIndex ( fun x -> not ( x = character ) )
          |> s.Substring
    
    let trimBack ( character : char ) ( s : string ) =
        s |> rev |> ( trimFront character ) |> rev
    
    let trim ( character : char ) ( s : string ) =
        s |> ( trimFront character ) |> ( trimBack character )
    