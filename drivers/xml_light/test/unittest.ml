module Driver = struct
  include Protocol_conv_xml.Xml_light
  let of_driver_exn _ = failwith "ignore"
  let of_driver _ = failwith "ignore"
  let to_driver _ = failwith "ignore"
  let sexp_of_t : t -> Sexplib.Sexp.t = fun t ->
    Sexplib.Std.sexp_of_string (to_string_hum t)
end
module Unittest = Test.Unittest.Make(Driver)
let () = Unittest.run ~name:"xml_light" ()
