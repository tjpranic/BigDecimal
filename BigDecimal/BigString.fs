namespace BigDecimal

module BigString =

    //String that can be indexed into using BigIntegers
    //TODO: implement this
    type BigString( words : string ) =
        
        new( ) = BigString( "" )