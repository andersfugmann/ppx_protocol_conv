(* Test all drivers *)
open !Base
open Protocol_conv_json
open Protocol_conv_xml
open Protocol_conv_msgpack




module Record : Util.Testable = struct
  let name = "record"
  type t1 = { x: int; y: string }
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]
  type v = A | B of int list * int list | C of string
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let v = B ([5; 6; 7], [10;11;12]);

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

  let t = {
    b = true;
    i = 2;
    i32 = Int32.of_int_exn 5;
    i64 = Int64.of_int_exn 10;
    f = 3.14;
    s = "string";
    il = [3; 4; 5];
    io = Some 100;
    t = (5, "protocol", false);
    v = [ v; v; v; ];
    x = { x = 5; y = "string" };
  }
end
let () = Util.test (module Record)

module List : Util.Testable = struct
  let name = "list"
  type t = { a: int list }
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]
  let t = { a = [1; 2; 3] }
end
let () = Util.test (module List)


module Lists : Util.Testable = struct
  let name = "Lists"

  type l = A of int list | B of int list list * int list * int | C of int list * int list
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  type t = {
    a: int list list;
    b: (int list * int list);
    l: l list;
  }
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = {
    a = [
      [ 1;2;3 ]; [4]
(* []   [10; 20; 30; 40];
      [100; 101]; *)
    ];

    b = ([8;9], [10;20;30;40]);

    l = [
      (*
      A [1;2;3];
      B ([[1;2]; [3;4;5]; [2]], [3;1], 5);
      C ([1;2;3], [3;4;5]);
      *)
    ]
  }

end
let () = Util.test (module Lists)
