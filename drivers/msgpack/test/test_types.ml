open Sexplib.Std
open Protocol_conv_msgpack

module Msgpack = struct
  include Msgpack
  let sexp_of_float32 = sexp_of_float
  let sexp_of_uint32 = sexp_of_int
  let sexp_of_uint64 = sexp_of_int
  let sexp_of_bytes = sexp_of_string
end


type t = {
  int: int;
  string: string;
  float: float;
  unit: unit;
  float32: Msgpack.float32;
  int32: int32;
  int64: int64;
  uint32: Msgpack.uint32;
  uint64: Msgpack.uint64;
  bytes: Msgpack.bytes;
}
[@@deriving protocol ~driver:(module Msgpack), sexp_of]

let test_types () =
  let t = {
    int = 0;
    string = "a";
    float = 0.0;
    unit = ();
    float32 = 0.0;
    int32 = Int32.one;
    int64 = Int64.one;
    uint32 = 0;
    uint64 = 0;
    bytes = "asd";
  }
  in
  let m = to_msgpack t in
  let t' = of_msgpack_exn m in
  let fmt : t Fmt.t = fun formatter t ->
    Format.fprintf formatter "%s" (Base.Sexp.to_string_hum (sexp_of_t t))
  in
  Alcotest.(check (of_pp fmt)) "Identity" t t';
  ()

let tests = __MODULE__, [ Alcotest.test_case "Msgpack types" `Quick test_types; ]
