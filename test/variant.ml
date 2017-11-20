open !Base
open !Protocol_conv_json
open !Protocol_conv_xml

module Simple = struct
  type v = A | B of int | C of int * int | D of (int * int)
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]

  type vs = v list
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]

  let () =
    let t = [ A; B 5; C (6,7); D (8,9) ] in
    Util.test_json "Variant.Simple" vs_to_json vs_of_json  t;
    Util.test_xml "Variant.Simple" vs_to_xml_light vs_of_xml_light t;
    ()
end

module Tree = struct
  type tree =
    | Node of tree * int * tree
    | Leaf
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]

  let () =
    let t = Node ( Node (Leaf, 3, Leaf), 10, Leaf) in
    Util.test_json "Variant.Tree" tree_to_json tree_of_json  t;
    Util.test_xml "Variant.Simple" tree_to_xml_light tree_of_xml_light t;
    ()
end

module Recursion = struct
  type v = V1 of v
         | V0 of int
         | U of u
  and u = | U1 of u
          | U2 of int
          | V of v
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]

  let () =
    let t = U (V (U (V (V1 (V0 5))))) in
    Util.test_json "Variant.Recursion" v_to_json v_of_json  t;
    Util.test_xml "Variant.Recursion" v_to_xml_light v_of_xml_light t;
    ()
end

(* Not supported yet
module Poly = struct
  type t = [ `A of int ]
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]
end
*)

(* Not supported yet
module Record = struct
  type v = V of { v1: int; v2: string}
         | U of { u1: string; u2: int }
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]

  let _ =
    let t = V { v1=5; v2="test" } in
    Util.test_json "Variant.Record" t_to_json t_of_json t;
    Util.test_xml "Variant.Record" t_to_xml_light t_of_xml_light t;
    ()


end
*)
