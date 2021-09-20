open System
open System.Numerics
open BigMath

// Ramanujan's formula for pi:
// 1 / pi = sqrt( 8 ) / 9801 * sum[n = 0->inf]( ( (4n)! / (n!)^4 ) * ( ( 26390n + 1103 ) / 396^(4n) ) ) )
//
// WolframAlpha query:
// 1 / ( ( sqrt( 8 ) / 9801 ) * sum ( ( 4n )! / ( n! )^4 ) * ( ( 26390n + 1103 ) / 396^( 4n ) ) n=0 to 5 )
//
// Pi = 3.1415926535897932384626433832795028841971693993...
let piRamanujan ( iterations : int32 ) =
    let reciprocalPi =
        BigDecimal.sqrt( BigDecimal( 8 ) ) / BigDecimal( 9801 ) *
        ( [ for k in 0I..BigInteger( iterations ) ->
            BigFraction( ( 4I * k ) |> BigInteger.factorial, k |> BigInteger.factorial |> BigInteger.pow 4I ) *
            BigFraction( ( 26390I * k ) + 1103I, 396I |> BigInteger.pow ( 4I * k ) ) ]
                |> List.sum
                |> BigFraction.toBigDecimal )
    BigDecimal( 1 ) / reciprocalPi

[<EntryPoint>]
let main( args : String[] ) =
    printfn "%A" ( piRamanujan 5 )
    printfn "%A" ( BigDecimal( 9167486769200391580986609275853801624831066801443086224071265164279346570408670965932792057674808067900227830163549248523803357453169351119035965775473400756816883056208210161291328455648I ) |> BigDecimal.nthrt 23 ) // 134522054.33137513807694048238669278806693778558635...
    0

