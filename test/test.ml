open Protocol_conv_json
open Protocol_conv_xml
open Protocol_conv_msgpack

module T : Util.Testable = struct
  let name = "Test.T"

  type a = string * int list
  and aopt = a option
  and  v = Variant_one of int [@key "Variant_two1"]
         | Variant_two of string
  and y = {
    y_a: int [@key "y_a"];
    y_b: a;
    y_c_: aopt [@key "y_yc"];
    y_d_: v [@key "y_yd"];
  }
  and  t = {
    foo: int;
    bar: string;
    baz: y;
  }
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = { foo=1;
            bar="one";
            baz={ y_a=2;
                  y_b=("two", [10; 20; 30]);
                  y_c_=Some ("three", [100; 200; 300]);
                  y_d_=Variant_one 1
                }
          }

end
let () = Util.test (module T)
