module Driver = struct
  let name = "xml_light"
  let serialize t = Xml.to_string t
  let deserialize s = Xml.parse_string s
  include Protocol_conv_xml.Xml_light
  let of_driver_exn = of_xml_light_exn
  let of_driver = of_xml_light
  let to_driver = to_xml_light
  let sexp_of_t : t -> Sexplib.Sexp.t = fun t ->
    Sexplib.Std.sexp_of_string (to_string_hum t)
end
module Unittest = Test.Unittest.Make(Driver)
let () = Unittest.run ()
