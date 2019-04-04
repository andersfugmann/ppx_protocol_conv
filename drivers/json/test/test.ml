open Protocol_conv_json

module Test : sig
  type ('a, 'b) u = X of ('a * 'b * 'a) | Y of { a: 'a; b:'b } | Z
  and t = A of (int, float) u | B of (float, string) u | C of { x: (float, char) u;
                                                                v: [ `A of (t, int) u | `B ] }
        | D | E of int
  [@@deriving protocol ~driver:(module Json)]

end = struct
  type ('a, 'b) u = X of ('a * 'b * 'a) | Y of { a: 'a; b:'b } | Z
  and t = A of (int, float) u | B of (float, string) u | C of { x: (float, char) u;
                                                                v: [ `A of (t, int) u | `B ] }
        | D | E of int
  [@@deriving protocol ~driver:(module Json)]

  let () =
    let t = A ( X (1,1.,2)) in
    let json = to_json t in
    let s = Yojson.Safe.pretty_to_string (json) in
    Printf.eprintf "%s - %!" s;
    let _ = of_json json in
    Printf.eprintf "Ok\n";

    let t = C { x = Y { a=5.; b='c' }; v = `A (X (D,3,E 5)) } in
    let json = to_json t in
    let s = Yojson.Safe.pretty_to_string (json) in
    Printf.eprintf "%s - %!" s;
    let _ = of_json json in
    Printf.eprintf "Ok\n";
end

module Sig : sig
  type ('a, 'b) x = ('a * 'b * 'a)
  [@@deriving protocol ~driver:(module Json)]

  type t = A | B of (int * int) | C of int * int | D of int * (int * int) * int | E of { a: int; b: int } | F of (int, int) x
  [@@deriving protocol ~driver:(module Json)]

end = struct

  type ('a, 'b) x = ('a * 'b * 'a)
  [@@deriving protocol ~driver:(module Json)]

  type t = A | B of (int * int) | C of int * int | D of int * (int * int) * int | E of { a: int; b: int } | F of (int, int) x
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

    let x : (int,int) x = (4,5,6) in
    let t = F x in
    let json = to_json t in
    let s = Yojson.Safe.pretty_to_string (json) in
    Printf.eprintf "%s - %!" s;
    let _ = of_json json in
    Printf.eprintf "Ok\n";
    ()
end
