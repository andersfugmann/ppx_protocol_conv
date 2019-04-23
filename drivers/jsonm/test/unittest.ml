module Driver = struct
  include Protocol_conv_jsonm.Jsonm
  let of_driver_exn = of_jsonm_exn
  let of_driver = of_jsonm
  let to_driver = to_jsonm
  let sexp_of_t : t -> Sexplib.Sexp.t = fun t ->
    Sexplib.Std.sexp_of_string (to_string_hum t)
end
module Unittest = Test.Unittest.Make (Driver)
let () = Unittest.run ~name:"jsonm" ()
