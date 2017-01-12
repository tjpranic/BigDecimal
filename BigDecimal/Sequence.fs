namespace System

[<RequireQualifiedAccess>]
module Seq =
    
    //break a given seq into groups of a given size
    let group ( s : 'a seq ) ( size : int32 ) =
        s |> Seq.mapi ( fun i x -> i / size, x )
          |> Seq.groupBy fst
          |> Seq.map ( fun ( _, x ) -> Seq.map snd x )
    