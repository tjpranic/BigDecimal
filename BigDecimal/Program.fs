open BigDecimal.BigDecimal
open BigDecimal.Fraction

[<EntryPoint>]
let main argv =
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

    let z = new BigDecimal( "550.0" )
    let y = new BigDecimal( "9.0" )

    printfn "%s" ( ( z.Pow( y ) ).ToString( ) ) //4605366583984375000000000
    
    printf "\n"

    let z = new BigDecimal( "8.0" )
    let y = new BigDecimal( "-2.0" )
    
    printfn "%s" ( ( z.Pow( y ) ).ToString( ) ) //0.015625

    0