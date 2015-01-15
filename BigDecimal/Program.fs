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
//
// pi = 3.1415926535897932384626433832795028841971693993
let pi_ramanujan( iterations : int ) =
    let reciprocal_pi =
        let first_terms = sqrt( bigdec( 8 ) ) / bigdec( 9801 )

        let second_terms( iterations : int ) =
            let sum_term_one( n : bigint ) =
                Fraction( fac( 4I * n ), pow( fac( n ), 4I ) )

            let sum_term_two( n : bigint ) =
                Fraction( ( 26390I * n ) + 1103I, pow( 396I, ( 4I * n ) ) )
            
            to_decimal(
                [ for k in 1..iterations -> sum_term_one( bigint( k ) ) *
                                            sum_term_two( bigint( k ) ) ]
                |> List.sum )

        ( first_terms * bigdec( 1103 ) ) +           //when n = 0
        ( first_terms * second_terms( iterations ) ) //when n > 0

    bigdec( 1 ) / reciprocal_pi

[<EntryPoint>]
let main argv =
    printfn "%A" ( pi_ramanujan( 15 ) )

    0