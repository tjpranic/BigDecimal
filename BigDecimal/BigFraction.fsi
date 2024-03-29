﻿namespace BigMath

open System
open System.Numerics

type BigFraction =

    static member Zero : BigFraction
    static member One  : BigFraction

    member public Numerator   : BigInteger
    member public Denominator : BigInteger

    static member (  + ) : BigFraction * BigFraction -> BigFraction
    static member (  - ) : BigFraction * BigFraction -> BigFraction
    static member (  * ) : BigFraction * BigFraction -> BigFraction
    static member (  / ) : BigFraction * BigFraction -> BigFraction
    static member (  + ) : BigFraction * BigInteger  -> BigFraction
    static member (  - ) : BigFraction * BigInteger  -> BigFraction
    static member (  * ) : BigFraction * BigInteger  -> BigFraction
    static member (  / ) : BigFraction * BigInteger  -> BigFraction
    static member (  + ) : BigInteger  * BigFraction -> BigFraction
    static member (  - ) : BigInteger  * BigFraction -> BigFraction
    static member (  * ) : BigInteger  * BigFraction -> BigFraction
    static member (  / ) : BigInteger  * BigFraction -> BigFraction
    static member ( ~- ) : BigFraction -> BigFraction

    static member op_Equality           : BigFraction * BigFraction -> bool
    static member op_Inequality         : BigFraction * BigFraction -> bool
    static member op_LessThan           : BigFraction * BigFraction -> bool
    static member op_LessThanOrEqual    : BigFraction * BigFraction -> bool
    static member op_GreaterThan        : BigFraction * BigFraction -> bool
    static member op_GreaterThanOrEqual : BigFraction * BigFraction -> bool

    interface IComparable

    override ToString    : unit -> String
    override Equals      : obj  -> bool
    override GetHashCode : unit -> int32

    new : BigInteger * BigInteger -> BigFraction
    new : int32      * int32      -> BigFraction
    new : int64      * int64      -> BigFraction
    new : uint32     * uint32     -> BigFraction
    new : uint64     * uint64     -> BigFraction
    new : unit                    -> BigFraction

[<RequireQualifiedAccess>]
[<CompilationRepresentation( CompilationRepresentationFlags.ModuleSuffix )>]
module BigFraction =

    val toBigDecimal : BigFraction -> BigDecimal
    val gcd          : BigInteger  -> BigInteger -> BigInteger
    val simplify     : BigFraction -> BigFraction