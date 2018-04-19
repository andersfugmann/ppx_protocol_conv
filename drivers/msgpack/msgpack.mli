(* Json Protocol *)
val mangle: string -> string

include Protocol_conv.Runtime.Driver with
  type t = Msgpck.t and
  type 'a flags = ?flags:[ `Mangle of (string -> string) ] -> 'a

val to_msgpack: t -> t
val of_msgpack: t -> t
