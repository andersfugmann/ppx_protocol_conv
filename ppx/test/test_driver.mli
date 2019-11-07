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
  | Bytes of bytes
  | Float of float
  | Char of char
  | Bool of bool
  | Unit

include Protocol_conv.Runtime.Driver with type  t := t
val of_test_exn: t -> t
val of_test: t -> (t, error) Protocol_conv.Runtime.result
val to_test: t -> t
