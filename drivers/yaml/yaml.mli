(* Json Protocol *)
include Protocol_conv.Runtime.Driver with
  type t = Global.Yaml.value and
  type 'a flags = ?flags:[ `Mangle of (string -> string) ] -> 'a

val of_yaml: t -> t
val to_yaml: t -> t
