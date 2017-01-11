namespace BigMath

open System
open System.Numerics

type BigDecimal =
    
    static member Precision : int32 with get, set
    
    static member Zero : BigDecimal
    static member One  : BigDecimal
    
    static member (  + ) : BigDecimal * BigDecimal -> BigDecimal
    static member (  - ) : BigDecimal * BigDecimal -> BigDecimal
    static member (  * ) : BigDecimal * BigDecimal -> BigDecimal
    static member (  / ) : BigDecimal * BigDecimal -> BigDecimal
    static member ( ** ) : BigDecimal * BigInteger -> BigDecimal
    static member ( ~- ) : BigDecimal -> BigDecimal
    
    static member Pow : BigDecimal * BigInteger -> BigDecimal
    static member Abs : BigDecimal -> BigDecimal
    
    static member op_Equality           : BigDecimal * BigDecimal -> bool
    static member op_Inequality         : BigDecimal * BigDecimal -> bool
    static member op_LessThan           : BigDecimal * BigDecimal -> bool
    static member op_LessThanOrEqual    : BigDecimal * BigDecimal -> bool
    static member op_GreaterThan        : BigDecimal * BigDecimal -> bool
    static member op_GreaterThanOrEqual : BigDecimal * BigDecimal -> bool
    
    interface IComparable
    
    override ToString    : unit -> String
    override Equals      : obj  -> bool
    override GetHashCode : unit -> int32
    
    new : String     -> BigDecimal
    new : decimal    -> BigDecimal
    new : double     -> BigDecimal
    new : int32      -> BigDecimal
    new : int64      -> BigDecimal
    new : uint32     -> BigDecimal
    new : uint64     -> BigDecimal
    new : BigInteger -> BigDecimal
    new : unit       -> BigDecimal

[<RequireQualifiedAccess>]
[<CompilationRepresentation( CompilationRepresentationFlags.ModuleSuffix )>]
module BigDecimal =
    
    val power        : BigDecimal -> BigInteger -> BigDecimal
    val nthRoot      : BigDecimal -> int32      -> BigDecimal
    val squareRoot   : BigDecimal -> BigDecimal
    val cubeRoot     : BigDecimal -> BigDecimal
    val abs          : BigDecimal -> BigDecimal
    val floor        : BigDecimal -> BigDecimal
    val ceiling      : BigDecimal -> BigDecimal
    val round        : BigDecimal -> BigDecimal
    val isDecimal    : BigDecimal -> bool
    val isWhole      : BigDecimal -> bool
    val toBigInteger : BigDecimal -> BigInteger