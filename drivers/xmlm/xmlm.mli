include Protocol_conv.Runtime.Driver with type t = Ezxmlm.node

val of_xmlm_exn: t -> t
val of_xmlm: t -> (t, error) Protocol_conv.Runtime.result
val to_xmlm: t -> t
