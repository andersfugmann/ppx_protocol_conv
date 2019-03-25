include Protocol_conv.Runtime.Driver with type t = Ezjsonm.value
module Make(P: Ppx_protocol_driver.Parameters) : (Protocol_conv.Runtime.Driver with type t = Ezjsonm.value)
val of_jsonm: t -> t
val to_jsonm: t -> t
