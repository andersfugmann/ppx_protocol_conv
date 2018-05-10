module TestDriver = struct
  open Protocol_conv_msgpack.Msgpack
  type nonrec t = t
  type 'a flags = 'a
  exception Protocol_error of string * t
  let to_variant x = to_variant x
  let of_variant x = of_variant x
  let to_record x = to_record x
  let of_record x = of_record x
  let to_tuple x = to_tuple x
  let of_tuple x = of_tuple x
  let to_option x = to_option x
  let of_option x = of_option x
  let to_list x = to_list x
  let of_list x = of_list x
  let to_lazy_t x = to_lazy_t x
  let of_lazy_t x = of_lazy_t x
  let to_int x = to_int x
  let of_int x = of_int x
  let to_int32 x = to_int32 x
  let of_int32 x = of_int32 x
  let to_int64 x = to_int64 x
  let of_int64 x = of_int64 x
  let to_string x = to_string x
  let of_string x = of_string x
  let to_float x = to_float x
  let of_float x = of_float x
  let to_bool x = to_bool x
  let of_bool x = of_bool x
  let to_unit x = to_unit x
  let of_unit x = of_unit x
end

module Unittest = Test.Unittest.Make (TestDriver)
let () = Unittest.run ~name:"msgpack"
