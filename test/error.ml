type x = int

(* Generalized algebraic datatypes not supported *)
type _ t = Int: int -> int t

(* Extensible variant types not supported *)
type attr = ..
type attr += Str of string
type attr += Int of int | Float of float

(* Functions not supported *)
type f = int -> int

(* Inline records not supported *)
type v = V of { a: int }

type p = [ `A ]

(* Inherited polymophic variants not supported *)
type q = [ p | `B ]

type u = private A | B of int
(* [@@deriving protocol ~driver:(module Driver)] *)
