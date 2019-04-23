include Protocol_conv.Runtime.Driver with type t = Xml.xml

val of_xml_light_exn: t -> t
val of_xml_light: t -> (t, error) Protocol_conv.Runtime.result
val to_xml_light: t -> t
