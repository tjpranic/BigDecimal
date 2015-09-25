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
    //printfn "%A\n" ( pi_ramanujan( 15 ) )

    //BigDecimal.MaxPrecision <- 300
    //let n = "9167486769200391580986609275853801624831066801443086224071265164279346570408670965932792057674808067900227830163549248523803357453169351119035965775473400756816883056208210161291328455648"
    //try
    //    printfn "%A" ( nth_root( 23, bigdec( n ) ) )
    //with
    //| ex -> printfn "%A\n%A" ex.Message ex.StackTrace

    //printfn "\n%A" ( [ for i in 0..63 -> bigdec( 2 ) ** bigint( i ) ] |> List.sum )
    //printfn "%A" ( bigdec( 2 ) ** 64I )

    0