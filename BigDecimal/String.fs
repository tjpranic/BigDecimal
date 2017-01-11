namespace System

[<RequireQualifiedAccess>]
module String =
    
    open System
    
    let rev ( s : string ) =
        new string( s.ToCharArray( ) |> Array.rev )
    
    //break a given string into groups of a given size
    let group ( s : string ) ( size : int ) =
        let rec loop ( number : string ) ( groups : string list ) =
            if number.Length = 0 then
                groups |> List.rev
            else
                let group, number =
                    if size < number.Length then
                        ( number.Substring( 0, size ), number.Substring( size ) )
                    else
                        ( number, "" )
                loop number ( group :: groups )
        loop s []
    
    let trimFront ( s : string ) ( character : char ) =
        let chars = s.ToCharArray( )
        let index = chars |> Array.tryFindIndex ( fun x -> not ( x = character ) )
        match index with
        | None       -> ""
        | Some index -> new string( Array.sub chars index ( s.Length - index ) )
    
    let trimBack ( s : string ) ( character : char ) =
        rev ( trimFront ( rev s ) character )
    
    let trim ( s : string ) ( character : char ) =
        trimBack ( trimFront s character ) character