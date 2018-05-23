include Protocol_conv.Runtime.Driver with
  type t = Ezjsonm.value and
  type 'a flags = ?flags:[ `Mangle of (string -> string) ] -> 'a

val of_jsonm: t -> t
val to_jsonm: t -> t
