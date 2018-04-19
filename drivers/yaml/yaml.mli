open Protocol_conv.Runtime
include Protocol_conv.Runtime.Driver with
  type t = Global.Yaml.value and
  type 'a flags = 'a no_flags

val of_yaml: t -> t
val to_yaml: t -> t
