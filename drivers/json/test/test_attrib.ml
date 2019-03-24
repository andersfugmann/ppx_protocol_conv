(* Extra tests *)
open Base
open OUnit2
open Protocol_conv_json
type u = A of int [@name "AAA"] | B of string [@name "BBB"]
[@@deriving protocol ~driver:(module Json), sexp]
type t = {
  i : int [@name "Integer"];
  u: u [@key "Poly"];
} [@@deriving protocol ~driver:(module Json), sexp]

let test_attrib _ =
  let t = { i = 5; u = A 3; } in
  let j = `Assoc [("i", `Int 5); ("Poly", `List [`String "AAA"; `Int 3])] in
  assert_equal t (j |> of_json);
  assert_equal ~printer:Yojson.Safe.to_string (to_json t) j;

  let t = { i = 5; u = B "abc"; } in
  let j = `Assoc [("i", `Int 5); ("Poly", `List [`String "BBB"; `String "abc"])] in
  assert_equal t (j |> of_json);
  assert_equal ~printer:Yojson.Safe.to_string (to_json t) j;
  ()

let test = "attrib test" >:: test_attrib
