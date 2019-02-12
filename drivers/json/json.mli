(* Json Protocol *)
include Protocol_conv.Runtime.Driver with
  type t = Yojson.Safe.t and
  type 'a flags = ?flags:[ `Mangle of (string -> string) ] -> 'a

val of_json: t -> t
val to_json: t -> t
