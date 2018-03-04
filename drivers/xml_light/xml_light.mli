open Protocol_conv.Runtime
include Driver with type t = Xml.xml and type 'a flags = 'a no_flags

val t_of_xml_light: t -> t
val t_to_xml_light: t -> t
