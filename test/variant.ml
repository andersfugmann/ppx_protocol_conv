open !Base
open !Deriving_protocol_json
open !Deriving_protocol_xml

module Simple = struct
  type v = A | B of int | C of int * int | D of (int * int)
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]

  type vs = v list
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]

  let _test_simple_variant : unit =
    let t = [ A; B 5; C (6,7); D (8,9) ] in
    let json = vs_to_json t in
    Caml.Printf.printf "%s\n" (Yojson.Safe.to_string json);
    assert (Base.Poly.equal t (vs_of_json json))
end

module Tree = struct
  type tree =
    | Node of tree * int * tree
    | Leaf
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]


  let _test_tree : unit =
    let t = Node ( Node (Leaf, 3, Leaf), 10, Leaf) in
    let json = tree_to_json t in
    Caml.Printf.printf "%s\n" (Yojson.Safe.to_string json);
    assert (Base.Poly.equal t (tree_of_json json))
end

module Recursion = struct
  type v = V1 of v
         | V0 of int
         | U of u
  and u = | U1 of u
          | U2 of int
          | V of v
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]

  let _test_recursion: unit =
    let t = U (V (U (V (V1 (V0 5))))) in
    let json = v_to_json t in
    Caml.Printf.printf "%s\n" (Yojson.Safe.to_string json);
    assert (Base.Poly.equal t (v_of_json json))
end

module Record = struct
  type v = V of { v1: int; v2: string}
         | U of { u1: string; u2: int }
end
