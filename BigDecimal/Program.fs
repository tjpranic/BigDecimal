open System
open System.Numerics
open Math.BigDecimal
open Math.BigFraction
open Math.BigInteger
open Utility.String

//Ramanujan's formula:
//1 / pi = sqrt( 8 ) / 9801 * sum[0->inf]( ( (4n)! / (n!)^4 ) * ( ( 26390n + 1103 ) / 396^(4n) ) ) )
//
//WolframAlpha query:
//1 / ( ( sqrt( 8 ) / 9801 ) * sum ( ( 4n )! / ( n! )^4 ) * ( ( 26390n + 1103 ) / 396^( 4n ) ) n=0 to 5 )
//
//Pi:
//3.1415926535897932384626433832795028841971693993...
let PiRamanujan ( iterations : int ) =
    let reciprocalPi =
        let term1 = BigDecimal.squareRoot( BigDecimal( 8 ) ) / BigDecimal( 9801 )
        let term2 ( iterations : int ) =
            [ for k in 1I..BigInteger( iterations ) ->
                BigFraction( ( BigInteger.factorial ( 4I * k ) ), ( BigInteger.power ( BigInteger.factorial k ) 4I ) ) *
                BigFraction( ( 26390I * k ) + 1103I, ( BigInteger.power 396I ( 4I * k ) ) ) ]
                    |> List.sum
                    |> BigFraction.toBigDecimal
        ( term1 * BigDecimal( 1103 ) ) + ( term1 * ( term2 iterations ) )
    BigDecimal( 1 ) / reciprocalPi

//TODO: add tests (test equality operators, .Equals, .CompareTo in BigFraction, BigDecimal)
[<EntryPoint>]
let main( args : String[] ) =
    printfn "%A\n" ( PiRamanujan 5 )
    printfn "%A\n" ( BigDecimal.nthRoot ( BigDecimal( 9167486769200391580986609275853801624831066801443086224071265164279346570408670965932792057674808067900227830163549248523803357453169351119035965775473400756816883056208210161291328455648I ) ) 23 ) //134522054.33137513807694048238669278806693778558635...
    0