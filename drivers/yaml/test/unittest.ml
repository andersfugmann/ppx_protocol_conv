module Driver = Protocol_conv_yaml.Yaml
module TestDriver = struct
  type t = Driver.t
  type 'a flags = 'a
  exception Protocol_error of string * t
  let to_variant x = Driver.to_variant x
  let of_variant x = Driver.of_variant x
  let to_record x = Driver.to_record x
  let of_record x = Driver.of_record x
  let to_tuple x = Driver.to_tuple x
  let of_tuple x = Driver.of_tuple x
  let to_option x = Driver.to_option x
  let of_option x = Driver.of_option x
  let to_list x = Driver.to_list x
  let of_list x = Driver.of_list x
  let to_array x = Driver.to_array x
  let of_array x = Driver.of_array x
  let to_lazy_t x = Driver.to_lazy_t x
  let of_lazy_t x = Driver.of_lazy_t x
  let to_int x = Driver.to_int x
  let of_int x = Driver.of_int x
  let to_int32 x = Driver.to_int32 x
  let of_int32 x = Driver.of_int32 x
  let to_int64 x = Driver.to_int64 x
  let of_int64 x = Driver.of_int64 x
  let to_string x = Driver.to_string x
  let of_string x = Driver.of_string x
  let to_float x = Driver.to_float x
  let of_float x = Driver.of_float x
  let to_bool x = Driver.to_bool x
  let of_bool x = Driver.of_bool x
  let to_unit x = Driver.to_unit x
  let of_unit x = Driver.of_unit x
end

module Unittest = Test.Unittest.Make (TestDriver)
let () = Unittest.run ~name:"yaml"
