module Yaml = Global.Yaml
module Driver : Ppx_protocol_driver.Driver with type t = Yaml.value = struct
  type t = Yaml.value
  let to_string_hum t =
    Yaml.to_string_exn t

  let of_list l = `A l
  let to_list = function `A l -> l | _ -> failwith "List expected"
  let is_list = function `A _ -> true | _ -> false

  let of_alist a = `O a
  let to_alist = function `O a -> a | _ -> failwith "Object expected"
  let is_alist = function `O _ -> true | _ -> false

  let of_int i = `Float (float_of_int i)
  let to_int = function
    | `Float f -> begin match modf f with
        | (f, i) when f <= epsilon_float -> int_of_float i
        | _ -> failwith "Int expected, got float"
      end
    | _ -> failwith "Int expected"

  let of_int32 i = Int32.to_int i |> of_int
  let to_int32 t = to_int t |> Int32.of_int

  let of_int64 i = Int64.to_int i |> of_int
  let to_int64 t = to_int t |> Int64.of_int

  let of_nativeint v = Nativeint.to_int v |> of_int
  let to_nativeint t = to_int t |> Nativeint.of_int

  let of_float f = `Float f
  let to_float = function `Float f -> f
                        | _ -> failwith "Float expected"

  let of_string s = `String s
  let to_string = function `String s -> s
                         | _ -> failwith "String expected"
  let is_string = function `String _ -> true | _ -> false

  let of_char c = of_string (String.make 1 c)
  let to_char t = match to_string t with
    | s when String.length s = 1 -> s.[0]
    | _ -> failwith "Got string with length != 1 when reading type 'char'"

  let of_bool b = `Bool b
  let to_bool = function `Bool b -> b
                       | _ -> failwith "Bool expected"

  let null = `Null
  let is_null = function `Null -> true | _ -> false
end
module Make(P: Ppx_protocol_driver.Parameters) = Ppx_protocol_driver.Make(Driver)(P)
include Ppx_protocol_driver.Make(Driver)(Ppx_protocol_driver.Default_parameters)

let of_yaml t = t
let to_yaml t = t
