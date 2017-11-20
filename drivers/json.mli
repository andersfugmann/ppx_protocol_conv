(* Json Protocol *)
val mangle: string -> string

include Protocol_conv.Runtime.Driver with
  type t = Yojson.Safe.json and
  type 'a flags = ?flags:[ `Mangle of (string -> string) ] -> 'a

val t_of_json: t -> t
val t_to_json: t -> t
