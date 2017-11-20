open !Base
open !Deriving_protocol_json
open !Deriving_protocol_xml


type a = string * int list [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]
type aopt = a option [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]

type y = {
  y_a: int [@key "y_ya"];
  y_b: a;
  y_c_: aopt [@key "y_yc"];
} [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]

type t = {
  foo: int;
  bar: string;
  baz: y;
} [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]

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
