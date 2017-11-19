(* Json Protocol *)
val mangle: string -> string

include Deriving_protocol.Runtime.Driver with
  type t = Msgpck.t and
  type 'a flags = ?flags:[ `Mangle of (string -> string) ] -> 'a
