open Protocol_conv_json
module JsonTestDriver = struct
  type t = Json.t
  type 'a flags = 'a
  exception Protocol_error of string * t
  let to_variant x = Json.to_variant x
  let of_variant x = Json.of_variant x
  let to_record x = Json.to_record x
  let of_record x = Json.of_record x
  let to_tuple x = Json.to_tuple x
  let of_tuple x = Json.of_tuple x
  let to_option x = Json.to_option x
  let of_option x = Json.of_option x
  let to_ref x = Json.to_ref x
  let of_ref x = Json.of_ref x
  let to_list x = Json.to_list x
  let of_list x = Json.of_list x
  let to_array x = Json.to_array x
  let of_array x = Json.of_array x
  let to_lazy_t x = Json.to_lazy_t x
  let of_lazy_t x = Json.of_lazy_t x
  let to_int x = Json.to_int x
  let of_int x = Json.of_int x
  let to_int32 x = Json.to_int32 x
  let of_int32 x = Json.of_int32 x
  let to_int64 x = Json.to_int64 x
  let of_int64 x = Json.of_int64 x
  let to_string x = Json.to_string x
  let of_string x = Json.of_string x
  let to_float x = Json.to_float x
  let of_float x = Json.of_float x
  let to_bool x = Json.to_bool x
  let of_bool x = Json.of_bool x
  let to_char x = Json.to_char x
  let of_char x = Json.of_char x
  let to_unit x = Json.to_unit x
  let of_unit x = Json.of_unit x
end

module Unittest = Test.Unittest.Make(JsonTestDriver)

(* Extra tests *)
open Base
open OUnit2
type u = A of int [@name "AAA"] | B of string [@name "BBB"]
[@@deriving protocol ~driver:(module Json), sexp]
type t = {
  i : int [@name "Integer"];
  u: u [@key "Poly"];
} [@@deriving protocol ~driver:(module Json), sexp]

let attrib_test _ =
  let t = { i = 5; u = A 3; } in
  let j = `Assoc [("i", `Int 5); ("Poly", `List [`String "AAA"; `Int 3])] in
  assert_equal t (j |> of_json);
  assert_equal ~cmp:Yojson.Safe.equal ~printer:Yojson.Safe.to_string (to_json t) j;

  let t = { i = 5; u = B "abc"; } in
  let j = `Assoc [("i", `Int 5); ("Poly", `List [`String "BBB"; `String "abc"])] in
  assert_equal t (j |> of_json);
  assert_equal ~cmp:Yojson.Safe.equal ~printer:Yojson.Safe.to_string (to_json t) j;
  ()

let () = Unittest.run ~extra:["attrib test" >:: attrib_test] ~name:"json" ()
