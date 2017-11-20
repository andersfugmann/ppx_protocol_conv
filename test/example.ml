open Base
open Protocol_conv_json


type a = string * int list [@@deriving protocol ~driver:(module Json)]
type aopt = a option [@@deriving protocol ~driver:(module Json)]

type y = {
  y_a: int [@key "y_ya"];
  y_b: a;
  y_c_: aopt [@key "y_yc"];
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
              }
        }

let _test_json : unit =
  Util.test_json Caml.__MODULE__ t_to_json t_of_json v;
  ()
