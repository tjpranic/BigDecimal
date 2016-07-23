open FSharpMath.BigDecimal
open FSharpMath.BigFraction
open FSharpMath.BigInteger

open System
open System.Numerics

// 1/pi = sqrt( 8 ) / 9801 *
//        sum[0->inf]( 
//            ( (4n)! / (n!)^4 ) * 
//            ( ( 26390n + 1103 ) / 396^(4n) ) ) 
//        )
// Wolfram Alpha Query: 
// 1 / ( (sqrt(8) / 9801) * sum ((4n)! / (n!)^4) * ((26390n + 1103) / 396^(4n)) n=0 to 5 )
//
// pi = 3.1415926535897932384626433832795028841971693993
let pi_ramanujan ( iterations : int ) =
    let BigFraction_to_decimal( n : BigFraction ) =
        if n.Numerator <> 0I && n.Denominator <> 0I then
                BigDecimal( n.Numerator ) / BigDecimal( n.Denominator )
            else
                BigDecimal( 0 )
    
    let reciprocal_pi =
        let first_terms = BigDecimal( 8 ).sqrt( ) / BigDecimal( 9801 )
        
        let second_terms ( iterations : int ) =
            let sum_term_one ( n : bigint ) =
                BigFraction( ( 4I * n ).factorial( ), n.factorial( ).power( 4I ) )
                
            let sum_term_two ( n : bigint ) =
                BigFraction( ( 26390I * n ) + 1103I, 396I.power( 4I * n ) )
                
            BigFraction_to_decimal ( [ for k in 1..iterations -> sum_term_one ( BigInteger( k ) ) * sum_term_two ( BigInteger( k ) ) ] |> List.sum )
            
        ( first_terms * BigDecimal( 1103 ) ) +    //when n = 0
        ( first_terms * second_terms iterations ) //when n > 0
        
    BigDecimal( 1 ) / reciprocal_pi

[<EntryPoint>]
let main( argv : String[] ) =
    printfn "%A\n" ( pi_ramanujan 5 )
    
    0