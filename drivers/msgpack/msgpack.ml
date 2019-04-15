module Driver : Ppx_protocol_driver.Driver with type t = Msgpck.t = struct
  type t = Msgpck.t

  let to_string_hum t =
    let b = Buffer.create 64 in
    Msgpck.pp (Format.formatter_of_buffer b ) t;
    Buffer.contents b

  let of_list = Msgpck.of_list
  let to_list = Msgpck.to_list
  let is_list = function Msgpck.List _ -> true | _ -> false

  let of_alist alist = List.map (fun (k, v) -> Msgpck.of_string k, v) alist |> Msgpck.of_map
  let to_alist t = Msgpck.to_map t |> List.map (fun (k, v) -> (Msgpck.to_string k, v))

  let is_alist = function Msgpck.Map _ -> true | _ -> false

  let to_int = function Msgpck.Int i -> i
                      | Msgpck.Int32 i -> Int32.to_int i
                      | Msgpck.Uint32 i -> Int32.to_int i
                      | Msgpck.Int64 i -> Int64.to_int i
                      | Msgpck.Uint64 i -> Int64.to_int i
                      | _ -> failwith "int expected"

  let of_int = Msgpck.of_int

  let of_int32 = Msgpck.of_int32
  let to_int32 = Msgpck.to_int32

  let of_int64 = Msgpck.of_int64
  let to_int64 = Msgpck.to_int64

  let of_nativeint v = Nativeint.to_int v |> of_int
  let to_nativeint t = to_int t |> Nativeint.of_int

  let to_float = function Msgpck.Float f -> f
                        | Msgpck.Float32 i -> Int32.float_of_bits i
                        | _ -> failwith "float expected"
  let of_float = Msgpck.of_float

  let of_string = Msgpck.of_string
  let to_string = Msgpck.to_string
  let is_string = function Msgpck.String _ -> true | _ -> false

  let of_char c = of_string (String.make 1 c)
  let to_char t = match to_string t with
    | s when String.length s = 1 -> s.[0]
    | _ -> failwith "Got string with length != 1 when reading type 'char'"

  let of_bool = Msgpck.of_bool
  let to_bool = Msgpck.to_bool

  let null = Msgpck.Nil
  let is_null = function Msgpck.Nil -> true | _ -> false
end
include Ppx_protocol_driver.Make(Driver)(Ppx_protocol_driver.Default_parameters)
module Make(P: Ppx_protocol_driver.Parameters) = Ppx_protocol_driver.Make(Driver)(P)

type nonrec bytes = string
let bytes_of_msgpack_exn = Msgpck.to_bytes
let bytes_to_msgpack = Msgpck.of_bytes

type uint32 = int
let uint32_of_msgpack_exn t = Msgpck.to_uint32 t |> Int32.to_int
let uint32_to_msgpack v = Int32.of_int v |> Msgpck.of_uint32

type uint64 = int
let uint64_of_msgpack_exn t = Msgpck.to_uint64 t |> Int64.to_int
let uint64_to_msgpack v = Int64.of_int v |> Msgpck.of_uint64

type float32 = float
let float32_of_msgpack_exn t = Msgpck.to_float32 t |> Int32.float_of_bits
let float32_to_msgpack v = Int32.bits_of_float v |> Msgpck.of_float32

let of_msgpack_exn t = t
let to_msgpack t = t
