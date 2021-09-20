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

    val toBigInteger : BigDecimal -> BigInteger
    val isDecimal    : BigDecimal -> bool
    val isWhole      : BigDecimal -> bool
    val parse        : string     -> BigDecimal
    val pow          : BigInteger -> BigDecimal -> BigDecimal
    val nthrt        : int32      -> BigDecimal -> BigDecimal
    val sqrt         : BigDecimal -> BigDecimal
    val cbrt         : BigDecimal -> BigDecimal
    val abs          : BigDecimal -> BigDecimal
    val floor        : BigDecimal -> BigDecimal
    val ceil         : BigDecimal -> BigDecimal
    val round        : int32      -> BigDecimal -> BigDecimal
    val whole        : BigDecimal -> BigInteger
    val fractional   : BigDecimal -> BigDecimal