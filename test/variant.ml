open Protocol_conv_json
open Protocol_conv_xml
open Protocol_conv_msgpack

module Simple : Util.Testable = struct
  let name = "Simple"

  type v = A | B of int | C of int * int | D of (int * int)
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  type t = v list
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = [ A; B 5; C (6,7); D (8,9) ]
end
let () = Util.test (module Simple)

module Tree = struct
  let name = "Tree"
  type t =
    | Node of t * int * t
    | Leaf
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = Node ( Node (Leaf, 3, Leaf), 10, Leaf)
end
let () = Util.test (module Tree)

module MutualRecursion : Util.Testable = struct
  let name = "MutualRecursion"
  type v = V1 of v
         | V0 of int
         | T of t
  and t = | T1 of t
          | T2 of int
          | V of v
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = T1 (V (T (V (V1 (V1 (V1 (V0 5)))))))
end
let () = Util.test (module MutualRecursion)

module InsideRec : Util.Testable = struct
  let name = "InsideRec"
  type v = V0 [@key "A"]
         | V1 [@key "B"]

  and t = { a : string;
            b : v; [@key "V"]
            c : string;
          }
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = { a= "a"; b = V0; c = "c" }
end
let () = Util.test (module InsideRec)
(*
module Poly = struct
  type t = [ `A of int ]
  [@@deriving to_protocol ~driver:(module Json)]
end
*)
(* Not supported yet
module Record = struct
  type v = V of { v1: int; v2: string}
         | U of { u1: string; u2: int }
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let _ =
    let t = V { v1=5; v2="test" } in
    Util.test_json "Variant.Record" t_to_json t_of_json t;
    Util.test_xml "Variant.Record" t_to_xml_light t_of_xml_light t;
    ()


end
*)
