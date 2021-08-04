module Driver = struct
  let serialize t = Msgpck.String.to_string t |> Bytes.to_string
  let deserialize t = Msgpck.String.read ~pos:0 t |> snd
  let name = "msgpack"
  include Protocol_conv_msgpack.Msgpack
  let of_driver_exn = of_msgpack_exn
  let of_driver = of_msgpack
  let to_driver = to_msgpack
  let sexp_of_t : t -> Sexplib.Sexp.t = fun t ->
    Sexplib.Std.sexp_of_string (to_string_hum t)
end
module Unittest = Test.Unittest.Make (Driver)
let () = Unittest.run ~extra:[Test_types.tests] ()
