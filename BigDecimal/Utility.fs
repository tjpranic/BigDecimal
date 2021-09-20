namespace BigMath

module Utility =
    
    let minmax x y =
        ( min x y, max x y )
    

namespace System

[<RequireQualifiedAccess>]
module Seq =
    
    //break a given seq into groups of a given size
    let group ( s : 'a seq ) ( size : int32 ) =
        s |> Seq.mapi ( fun i x -> i / size, x )
          |> Seq.groupBy fst
          |> Seq.map ( fun ( _, x ) -> Seq.map snd x )

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

namespace System.Numerics

[<RequireQualifiedAccess>]
module BigInteger =
    
    let factorial ( n : BigInteger ) =
        match n with
        | _ when n = 0I -> 1I
        | _ when n = 1I -> n
        | _             -> [ 1I..n ] |> List.reduce ( * )
    
    let pow ( x : BigInteger ) ( n : BigInteger ) =
        match x with
        | _ when x = 0I -> 1I
        | _ when x = 1I -> n
        | _             -> [ for i in 1I..x do yield n ] |> List.reduce ( * )