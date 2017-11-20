open Base
open Protocol_conv_json


type a = string * int list [@@deriving protocol ~driver:(module Json)]
type aopt = a option [@@deriving protocol ~driver:(module Json)]

type v = Variant_one of int [@key "VARIANT_ONE"]
       | Variant_two of string
[@@deriving protocol ~driver:(module Json)]

type y = {
  y_a: int [@key "y_a"];
  y_b: a;
  y_c_: aopt [@key "y_yc"];
  y_d_: v [@key "y_yd"];
} [@@deriving protocol ~driver:(module Json)]

type t = {

  foo: int;
  bar: string;
  baz: y;
} [@@deriving protocol ~driver:(module Json)]

let v = { foo=1;
          bar="one";
          baz={ y_a=2;
                y_b=("two", [10; 20; 30]);
                y_c_=Some ("three", [100; 200; 300]);
                y_d_=Variant_one 1
              }
        }

type u = {
  a: Json.t;
  b: Json.t;
  c: int;
} [@@deriving protocol ~driver:(module Json)]

let u = { a = `Int 5; b = `String "B"; c = 3 }

let _test_json : unit =
  Util.test_json Caml.__MODULE__ t_to_json t_of_json v;
  Util.test_json Caml.__MODULE__ u_to_json u_of_json u;
  ()
