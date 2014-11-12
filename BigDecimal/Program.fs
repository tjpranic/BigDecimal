open BigDecimal.BigDecimal
open BigDecimal.Fraction
open BigDecimal.Utility

// 1/pi = sqrt( 8 ) / 9801 *
//        sum[0->inf]( 
//            ( (4n)! / (n!)^4 ) * 
//            ( ( 26390n + 1103 ) / 396^(4n) ) ) 
//        )
// Wolfram Alpha Query: 
// 1 / ( (sqrt(8) / 9801) * sum ((4n)! / (n!)^4) * ((26390n + 1103) / 396^(4n)) n=0 to 5 )
let pi_ramanujan( ) =
    let max_iterations = 10I

    let first_terms = ( new BigDecimal( 8 ) ).Sqrt( ) / ( new BigDecimal( 9801 ) )

    let sum_one( n : bigint ) =
        new Fraction( fac( 4I * n ), pow( fac( n ), 4I ) )

    let sum_two( n : bigint ) =
        new Fraction( ( 26390I * n ) + 1103I, pow( 396I, ( 4I * n ) ) )

    let result =
        ( first_terms * ( new BigDecimal( 1103 ) ) ) + //iteration when n = 0
        ( first_terms * ( [ for k in 1I..max_iterations do yield sum_one( k ) * sum_two( k ) ] |> List.sum ).Decimal ) //iteration when n > 0

    ( new BigDecimal( 1 ) ) / result

[<EntryPoint>]
let main argv =
    let pi = pi_ramanujan( )
    printfn "%A" pi

    0