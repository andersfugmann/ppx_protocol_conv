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
  let j = [("i", `Int 5); ("Poly", `List [`String "AAA"; `Int 3])] |> List.rev in
  assert_equal t (`Assoc j |> of_json_exn);
  assert_equal ~printer:Yojson.Safe.to_string (to_json t) (`Assoc j);

  let t = { i = 5; u = B "abc"; } in
  let j = [("i", `Int 5); ("Poly", `List [`String "BBB"; `String "abc"])] |> List.rev in
  assert_equal t (`Assoc j |> of_json_exn);
  assert_equal ~printer:Yojson.Safe.to_string (to_json t) (`Assoc j);
  ()

let test = "attrib test" >:: test_attrib
