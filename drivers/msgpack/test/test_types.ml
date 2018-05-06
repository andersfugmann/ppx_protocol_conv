open Protocol_conv_msgpack
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
  t: Msgpack.t;
}
[@@deriving protocol ~driver:(module Msgpack)]

let () =
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
    t = Msgpck.Nil;
  }
  in
  let m = to_msgpack t in
  let t' = of_msgpack m in
  assert (t = t');
  Printf.printf ".\n";
  ()
