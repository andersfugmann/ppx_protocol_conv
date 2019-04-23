include Protocol_conv.Runtime.Driver with type t = Ezjsonm.value
module Make(P: Ppx_protocol_driver.Parameters) : (Protocol_conv.Runtime.Driver with type t = Ezjsonm.value)
val of_jsonm_exn: t -> t
val of_jsonm: t -> (t, error) Protocol_conv.Runtime.result
val to_jsonm: t -> t
