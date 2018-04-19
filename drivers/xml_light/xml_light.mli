open Protocol_conv.Runtime
include Driver with type t = Xml.xml and type 'a flags = 'a no_flags

val of_xml_light: t -> t
val to_xml_light: t -> t
