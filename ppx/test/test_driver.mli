type v =
  | Variant of (string * v list)
  | Record of (string * v) list
  | Tuple of (string * v) list
  | Option of v option
  | List of v list
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | String of string
  | Float of float
  | Bool of bool
  | Unit

include Protocol_conv.Runtime.Driver with
  type t = v and
  type 'a flags = 'a

val t_of_test: t -> t
val t_to_test: t -> t
