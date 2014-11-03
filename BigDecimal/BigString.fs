module BigString

//String that can be indexed into using BigIntegers
type BigString( words : string ) =
    
    new( ) = BigString( "" )