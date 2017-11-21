(* Test all drivers *)
open !Base
open Protocol_conv_json
open Protocol_conv_xml
open Protocol_conv_msgpack

type v = A | B of int list | C of string
[@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

type t1 = { x: int; y: string }
[@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

type t = {
  b: bool;
  i: int;
  i32: int32;
  i64: int64;
  f: float;
  s: string;
  il: int list;
  io: int option;
  t: (int * string * bool);
  v: v list;
  x: t1;

}
[@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

let v = {
  b = true;
  i = 2;
  i32 = Int32.of_int_exn 5;
  i64 = Int64.of_int_exn 10;
  f = 3.14;
  s = "string";
  il = [3; 4; 5];
  io = Some 100;
  t = (5, "protocol", false);
  v = [ A; B [5; 6; 7]; C "protocol"];
  x = { x = 5; y = "string" };
}

let () =
  Util.test_json Caml.__MODULE__ t_to_json t_of_json v;
  Util.test_xml Caml.__MODULE__ t_to_xml_light t_of_xml_light v;
  Util.test_msgpack Caml.__MODULE__ t_to_msgpack t_of_msgpack v;
  ()
