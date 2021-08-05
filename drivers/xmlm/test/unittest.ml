module Driver = struct
  let name = "xmlm"
  let serialize t = Ezxmlm.to_string [t]
  let deserialize t = Ezxmlm.from_string t |> snd |> List.hd
  include Protocol_conv_xmlm.Xmlm
  let of_driver_exn = of_xmlm_exn
  let of_driver = of_xmlm
  let to_driver = to_xmlm
  let sexp_of_t : t -> Sexplib.Sexp.t = fun t ->
    Sexplib.Std.sexp_of_string (to_string_hum t)
end
module Unittest = Test.Unittest.Make(Driver)
let () = Unittest.run()
