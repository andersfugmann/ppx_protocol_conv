module Driver : Ppx_protocol_driver.Driver with type t = Yojson.Safe.json = struct
  type t = Yojson.Safe.json
  module U = Yojson.Safe.Util

  let to_string_hum t =
    Yojson.Safe.pretty_to_string t

  let of_list l = `List l
  let to_list = U.to_list
  let is_list = function `List _ -> true | _ -> false

  let of_alist a = `Assoc a
  let to_alist = U.to_assoc
  let is_alist = function `Assoc _ -> true | _ -> false

  let of_int i = `Int i
  let to_int = U.to_int

  let of_int32 i = Int32.to_int i |> of_int
  let to_int32 t = to_int t |> Int32.of_int

  let of_int64 i = Int64.to_int i |> of_int
  let to_int64 t = to_int t |> Int64.of_int

  let of_float f = `Float f
  let to_float t = U.to_float t

  let of_string s = `String s
  let to_string = U.to_string
  let is_string = function `String _ -> true | _ -> false

  let of_bool b = `Bool b
  let to_bool = U.to_bool

  let null = `Null
  let is_null = function `Null -> true | _ -> false
end

include Ppx_protocol_driver.Make(Driver)

(* Allow referencing Json.t in structures. *)
let of_json t = t
let to_json t = t
