module Driver : Ppx_protocol_driver.Driver with type t = Yojson.Safe.json [@warning "-3"] = struct
  type t = Yojson.Safe.json [@warning "-3"]

  let to_string_hum t =
    Yojson.Safe.pretty_to_string t

  let of_list l = `List l
  let to_list = function `List l -> l | _ -> failwith "List expected"
  let is_list = function `List _ -> true | _ -> false

  let of_alist a = `Assoc a
  let to_alist = function `Assoc a -> a | _ -> failwith "Assoc expected"
  let is_alist = function `Assoc _ -> true | _ -> false

  let of_int i = `Int i
  let to_int = function `Int i -> i | _ -> failwith "Int expected"

  let of_int32 i = Int32.to_int i |> of_int
  let to_int32 t = to_int t |> Int32.of_int

  let of_int64 i = Int64.to_int i |> of_int
  let to_int64 t = to_int t |> Int64.of_int

  let of_nativeint i = Nativeint.to_int i |> of_int
  let to_nativeint t = to_int t |> Nativeint.of_int

  let of_float f = `Float f
  let to_float = function `Float f -> f | _ -> failwith "Float expected"

  let of_string s = `String s
  let to_string = function `String s -> s | _ -> failwith "String expected"
  let is_string = function `String _ -> true | _ -> false

  let of_char c = of_string (String.make 1 c)
  let to_char t = match to_string t with
    | s when String.length s = 1 -> s.[0]
    | _ -> failwith "Got string with length != 1 when reading type 'char'"

  let of_bool b = `Bool b
  let to_bool = function `Bool b -> b | _ -> failwith "Bool expected"

  let null = `Null
  let is_null = function `Null -> true | _ -> false
end

include Ppx_protocol_driver.Make(Driver)(Ppx_protocol_driver.Default_parameters)
module Make(P: Ppx_protocol_driver.Parameters) = Ppx_protocol_driver.Make(Driver)(P)

module Yojson = struct
  include Make(struct
      include Ppx_protocol_driver.Default_parameters
      let omit_default_values = true
      let constructors_without_arguments_as_string = false
      let eager = true
      let strict = true
    end)
  let to_yojson t = t
  let of_yojson t = t
end

(* Allow referencing Json.t in structures. *)
let of_json t = t
let to_json t = t
