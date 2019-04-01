open Protocol_conv_json

module Test_adt = struct
  type t = A | B of { a: int; b: float }
  [@@deriving protocol ~driver:(module Json)]

  type x = { t: t }
  [@@deriving protocol ~driver:(module Json)]

  let () =
    let t = B { a = 5; b = 6.0 } in
    let s = Yojson.Safe.pretty_to_string (x_to_json { t }) in
    Printf.eprintf "%s - %!" s;
    let _ = x_of_json (Yojson.Safe.from_string s) in
    Printf.eprintf "Ok\n";
    ()
end

module Test_tuple = struct
  type t = int * string
  and x = { t: t }
  [@@deriving protocol ~driver:(module Json)]

  let () =
    let t = (4, "abc") in
    let s = Yojson.Safe.pretty_to_string (x_to_json { t }) in
    Printf.eprintf "%s - %!" s;
    let _ = x_of_json (Yojson.Safe.from_string s) in
    Printf.eprintf "Ok\n";
    ()
end

module Test_poly = struct
  type t = [ `A | `B of int | `C of int * string | `D of (int * string) ]
  [@@deriving protocol ~driver:(module Json)]

  type x = { t: t }
  [@@deriving protocol ~driver:(module Json)]

  let () =
    let t = `C (4,"6") in
    let s = Yojson.Safe.pretty_to_string (x_to_json { t }) in
    Printf.eprintf "%s - %!" s;
    let _ = x_of_json (Yojson.Safe.from_string s) in
    Printf.eprintf "Ok\n";
    ()

    let () =
    let t = `D (4,"6") in
    let s = Yojson.Safe.pretty_to_string (x_to_json { t }) in
    Printf.eprintf "%s - %!" s;
    let _ = x_of_json (Yojson.Safe.from_string s) in
    Printf.eprintf "Ok\n";
    ()

  let () =
    let t = `A in
    let s = Yojson.Safe.pretty_to_string (x_to_json { t }) in
    Printf.eprintf "%s - %!" s;
    let _ = x_of_json (Yojson.Safe.from_string s) in
    Printf.eprintf "Ok\n";

    let s = Yojson.Safe.pretty_to_string (x_to_json { t }) in
    Printf.eprintf "%s - %!" s;
    let _ = x_of_json (Yojson.Safe.from_string s) in
    Printf.eprintf "Ok\n";
    ()

end

module Test_variant = struct
  type t = A | B of int | C of int * string | D of (int * string)
  [@@deriving protocol ~driver:(module Json)]

  type x = { t: t }
  [@@deriving protocol ~driver:(module Json)]

  let () =
    let t = C (4,"6") in
    let s = Yojson.Safe.pretty_to_string (x_to_json { t }) in
    Printf.eprintf "%s - %!" s;
    let _ = x_of_json (Yojson.Safe.from_string s) in
    Printf.eprintf "Ok\n";
    ()

    let () =
    let t = D (4,"6") in
    let s = Yojson.Safe.pretty_to_string (x_to_json { t }) in
    Printf.eprintf "%s - %!" s;
    let _ = x_of_json (Yojson.Safe.from_string s) in
    Printf.eprintf "Ok\n";
    ()

  let () =
    let t = A in
    let s = Yojson.Safe.pretty_to_string (x_to_json { t }) in
    Printf.eprintf "%s - %!" s;
    let _ = x_of_json (Yojson.Safe.from_string s) in
    Printf.eprintf "Ok\n";

    let s = Yojson.Safe.pretty_to_string (x_to_json { t }) in
    Printf.eprintf "%s - %!" s;
    let _ = x_of_json (Yojson.Safe.from_string s) in
    Printf.eprintf "Ok\n";
    ()
end
