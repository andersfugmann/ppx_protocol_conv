(* Extra tests *)
open Sexplib.Std
open Protocol_conv_json
type u = A of int [@name "AAA"] | B of string [@name "BBB"]
[@@deriving protocol ~driver:(module Json), sexp]
type t = {
  i : int [@name "Integer"];
  u: u [@key "Poly"];
} [@@deriving protocol ~driver:(module Json), sexp]

let fmt formatter t =
  Caml.Format.fprintf formatter "%s" (Base.Sexp.to_string_hum (sexp_of_t t))

let fmt_yojson formatter t =
  Caml.Format.fprintf formatter "%s" (Yojson.Safe.pretty_to_string t)


let test_attrib_name () =
  let t = { i = 5; u = A 3; } in
  let j = [("i", `Int 5); ("Poly", `List [`String "AAA"; `Int 3])] |> List.rev in
  Alcotest.(check (of_pp fmt)) "Deserialize" t (`Assoc j |> of_json_exn);
  Alcotest.(check (of_pp fmt_yojson)) "Serialize" (to_json t) (`Assoc j);
  ()

let test_attrib_key () =
  let t = { i = 5; u = B "abc"; } in
  let j = [("i", `Int 5); ("Poly", `List [`String "BBB"; `String "abc"])] |> List.rev in
  Alcotest.(check (of_pp fmt)) "Deserialize" t (`Assoc j |> of_json_exn);
  Alcotest.(check (of_pp fmt_yojson)) "Serialize" (to_json t) (`Assoc j);
  ()

let test =
    __MODULE__,
           [ Alcotest.test_case "Attrib name" `Quick test_attrib_name;
             Alcotest.test_case "Attrib key" `Quick test_attrib_key; ]
