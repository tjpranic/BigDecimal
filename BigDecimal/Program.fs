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
    let max_iterations = 15I

    let reciprocal_pi =
        let first_terms = sqrt( bigdec( 8 ) ) / bigdec( 9801 )

        let sum_term_one( n : bigint ) =
            Fraction( fac( 4I * n ), pow( fac( n ), 4I ) )

        let sum_term_two( n : bigint ) =
            Fraction( ( 26390I * n ) + 1103I, pow( 396I, ( 4I * n ) ) )

        first_terms * bigdec( 1103 ) + //when n = 0
        ( first_terms * get_decimal( [ for k in 1I..max_iterations do yield sum_term_one( k ) * sum_term_two( k ) ] |> List.sum ) ) //when n > 0

    bigdec( 1 ) / reciprocal_pi

[<EntryPoint>]
let main argv =
    let pi = pi_ramanujan( )
    printfn "%A" pi

    0