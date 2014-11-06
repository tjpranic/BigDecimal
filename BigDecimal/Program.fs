open BigDecimal.BigDecimal
open BigDecimal.Fraction
open BigDecimal.Utility

// 1/pi = sqrt( 8 ) / 9801 *
//        sum[0->inf]( 
//            ( (4n)! / (n!)^4 ) * 
//            ( ( 26390n + 1103 ) / 396^(4n) ) ) 
//        )
let pi_ramanujan( ) =
    let max_iterations = 5I

    let first_terms = new BigDecimal( sqrt( 8.0 ) / 9801.0 )

    let second_terms( n : bigint ) =
        ( new BigDecimal( fac( 4I * n ) ) ) / ( new BigDecimal( pow( fac( n ), 4I ) ) ) *
        ( new BigDecimal( ( 26390I * n ) + 1103I ) ) / ( new BigDecimal( pow( 396I, ( 4I * n ) ) ) )

    let result =
        ( first_terms * ( new BigDecimal( 1103 ) ) ) + //iteration when n = 0
        ( first_terms * ( [ for k in 1I..max_iterations do yield second_terms( k ) ] |> List.sum ) ) //iteration when n > 0

    ( new BigDecimal( 1 ) ) / result

[<EntryPoint>]
let main argv =
    let pi = pi_ramanujan( )
    printfn "%A" pi

    0

(*
let tests( ) =
    //Same scale, won't go up/down in scale
    let z = new BigDecimal( "45.11" )
    let y = new BigDecimal( "12.15" )

    printfn "%s" ( ( z + y ).ToString( ) ) //57.26
    printfn "%s" ( ( z - y ).ToString( ) ) //32.96
    printfn "%s" ( ( z * y ).ToString( ) ) //548.0865
    printfn "%s" ( ( z / y ).ToString( ) ) //3.7127572016460905349794238683... period 27

    printf "\n"

    //Same scale, will go/down up in scale
    let z = new BigDecimal( "100.001" )
    let y = new BigDecimal( "900.009" )

    printfn "%s" ( ( z + y ).ToString( ) ) //1000.01
    printfn "%s" ( ( z - y ).ToString( ) ) //-800.008
    printfn "%s" ( ( z * y ).ToString( ) ) //90001.800009
    printfn "%s" ( ( z / y ).ToString( ) ) //0.1... period 1

    printf "\n"

    //Different scale
    let z = new BigDecimal( "152684.6666667" )
    let y = new BigDecimal( "8200.99" )

    printfn "%s" ( ( z + y ).ToString( ) ) //160885.6566667
    printfn "%s" ( ( z - y ).ToString( ) ) //144483.6766667
    printfn "%s" ( ( z * y ).ToString( ) ) //1252165424.486940033
    printfn "%s" ( ( z / y ).ToString( ) ) //18.617-[not typing the rest]-... period 247

    printf "\n"

    //Powers
    let z = new BigDecimal( "550.0" )
    let y = new BigDecimal( "9.0" )

    printfn "%s" ( ( z.Pow( y ) ).ToString( ) ) //4605366583984375000000000
    
    printf "\n"

    //Negative powers (reciprocals)
    let z = new BigDecimal( "8.0" )
    let y = new BigDecimal( "-2.0" )
    
    printfn "%s" ( ( z.Pow( y ) ).ToString( ) ) //0.015625
*)