type t =
  | Record of (string * t) list
  | Variant of string * t list
  | Tuple of t list
  | Option of t option
  | List of t list
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | String of string
  | Float of float
  | Char of char
  | Bool of bool
  | Unit

include Protocol_conv.Runtime.Driver with type  t := t
val t_of_test: t -> t
val t_to_test: t -> t
