open Base
open Protocol_conv_json
open Protocol_conv_xml
open Protocol_conv_msgpack

open Caml.Printf

let () = Caml.at_exit (fun () -> printf "\n")

let verbose = (Array.length Caml.Sys.argv) > 1 &&
              String.equal Caml.Sys.argv.(1) "-v"

let test name ?printer to_p of_p t =
  if verbose then printf "json %s: %!" name;
  let p = try to_p t with e ->
    printf "\n%s: Unable to serialize.\n" name;
    raise e
  in
  let t' =
    try of_p p with
    | e ->
      printf "\n%s: Unable to de-serialize.\n" name;
      Option.iter printer ~f:(fun printer -> printf "data: %s\n" (printer p));
      raise e
  in
  match (Poly.equal t t', verbose) with
  | true, false -> Caml.print_string "."
  | true, true ->
    Option.iter printer ~f:(fun printer -> printf "OK: %s\n" (printer p));
  | false, _ ->
    printf "Error - Not equal";
    Option.iter printer ~f:(fun printer -> printf "data was: %s\n" (printer p));
    failwith "Not equal"

let test_json name = test ("Json: " ^ name) ~printer:Yojson.Safe.to_string
let test_xml name = test ("Xml: " ^ name) ~printer:Xml.to_string
let test_msgpack name = test ("Msgpack: " ^ name) ~printer:Msgpck.show

module type Testable = sig
  type t
  val t: t
  val name: string
  val t_of_json: Json.t -> t
  val t_to_json: t -> Json.t
  val t_of_xml_light: Xml_light.t -> t
  val t_to_xml_light: t -> Xml_light.t
  val t_of_msgpack: Msgpack.t -> t
  val t_to_msgpack: t -> Msgpack.t
end

let test (module T : Testable) =
  test_json T.name T.t_to_json T.t_of_json T.t;
  test_xml T.name T.t_to_xml_light T.t_of_xml_light T.t;
  test_msgpack T.name T.t_to_msgpack T.t_of_msgpack T.t;
  ()

let () = printf "Test %s:" Caml.Sys.argv.(0)
