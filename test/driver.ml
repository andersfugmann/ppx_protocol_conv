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
    bool: bool;
    int: int;
    int32: int32;
    int64: int64;
    float: float;
    string: string;
    intlist: int list;
    intoption: int option;
    tuple: (int * string * bool);
    vlist: v list;
    record: t1;
  }
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = {
    bool = true;
    int = 2;
    int32 = Int32.of_int_exn 5;
    int64 = Int64.of_int_exn 10;
    float = 3.14;
    string = "string";
    intlist = [3; 4; 5];
    intoption = Some 100;
    tuple = (5, "protocol", false);
    vlist = [ v; v; v; ];
    record = { x = 5; y = "string" };
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
