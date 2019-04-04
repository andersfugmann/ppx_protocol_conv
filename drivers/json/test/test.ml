open Protocol_conv_json

type t = A | B of (int * int) | C of int * int | D of int * (int * int) * int | E of { a: int; b: int }
[@@deriving protocol ~driver:(module Json)]

let () =
  let t = A in
  let json = to_json t in
  let s = Yojson.Safe.pretty_to_string (json) in
  Printf.eprintf "%s - %!" s;
  let _ = of_json json in
  Printf.eprintf "Ok\n";

  let t = B (4,5) in
  let json = to_json t in
  let s = Yojson.Safe.pretty_to_string (json) in
  Printf.eprintf "%s - %!" s;
  let _ = of_json json in
  Printf.eprintf "Ok\n";

  let t = C (4,5) in
  let json = to_json t in
  let s = Yojson.Safe.pretty_to_string (json) in
  Printf.eprintf "%s - %!" s;
  let _ = of_json json in
  Printf.eprintf "Ok\n";

  let t = D (4, (10,20), 5) in
  let json = to_json t in
  let s = Yojson.Safe.pretty_to_string (json) in
  Printf.eprintf "%s - %!" s;
  let _ = of_json json in
  Printf.eprintf "Ok\n";

  let t = E {a = 4; b = 5} in
  let json = to_json t in
  let s = Yojson.Safe.pretty_to_string (json) in
  Printf.eprintf "%s - %!" s;
  let _ = of_json json in
  Printf.eprintf "Ok\n";

  ()
