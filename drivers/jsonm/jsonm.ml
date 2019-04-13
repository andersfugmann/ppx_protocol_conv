let identity x = x

module Driver : Ppx_protocol_driver.Driver with type t = Ezjsonm.value = struct
  type t = Ezjsonm.value

  let to_string_hum t =
    Ezjsonm.wrap t |> Ezjsonm.to_string ~minify:false

  let of_list = Ezjsonm.list identity
  let to_list = Ezjsonm.get_list identity
  let is_list = function `A _ -> true | _ -> false

  let of_alist = Ezjsonm.dict
  let to_alist = Ezjsonm.get_dict
  let is_alist = function `O _ -> true | _ -> false

  let to_int = Ezjsonm.get_int
  let of_int = Ezjsonm.int

  let to_int32 = Ezjsonm.get_int32
  let of_int32 = Ezjsonm.int32

  let to_int64 = Ezjsonm.get_int64
  let of_int64 = Ezjsonm.int64

  let of_nativeint i = Nativeint.to_int i |> of_int
  let to_nativeint t = to_int t |> Nativeint.of_int

  let to_float = Ezjsonm.get_float
  let of_float = Ezjsonm.float

  let to_string = Ezjsonm.get_string
  let of_string = Ezjsonm.string
  let is_string = function `String _ -> true | _ -> false

  let of_char c = of_string (String.make 1 c)
  let to_char t = match to_string t with
    | s when String.length s = 1 -> s.[0]
    | _ -> failwith "Got string with length != 1 when reading type 'char'"

  let to_bool = Ezjsonm.get_bool
  let of_bool = Ezjsonm.bool

  let null = `Null
  let is_null = function `Null -> true | _ -> false
end

include Ppx_protocol_driver.Make(Driver)(Ppx_protocol_driver.Default_parameters)
module Make(P: Ppx_protocol_driver.Parameters) = Ppx_protocol_driver.Make(Driver)(P)

let of_jsonm = identity
let to_jsonm = identity
