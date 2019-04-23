module Driver = struct
  include Test_driver
  let of_driver_exn = of_test_exn
  let of_driver = of_test
  let to_driver = to_test
  let sexp_of_t : t -> Sexplib.Sexp.t = fun t ->
    Sexplib.Std.sexp_of_string (to_string_hum t)
end
module Unittest = Test.Unittest.Make (Driver)
let () = Unittest.run ~name:"ppx_test" ()
