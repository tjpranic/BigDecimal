namespace Utility

module String =
    
    open System
    
    type String with
        
        static member reverse ( s : string ) =
            new string( s.ToCharArray( ) |> Array.rev )
        
        //break a given string into groups of a given size
        static member group ( s : string ) ( size : int ) =
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