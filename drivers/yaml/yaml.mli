(** Yaml Protocol *)
module Make(P: Ppx_protocol_driver.Parameters) : (Protocol_conv.Runtime.Driver with type t = Global.Yaml.value)
include Protocol_conv.Runtime.Driver with type t = Global.Yaml.value
val of_yaml_exn: t -> t
val of_yaml: t -> (t, error) Protocol_conv.Runtime.result
val to_yaml: t -> t
