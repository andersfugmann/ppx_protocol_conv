open Protocol_conv_json
open Base
open Core_bench.Std

module Json = Json.Make(struct
    include Ppx_protocol_driver.Default_parameters
    let omit_default_values = true
    let strict = true
    let constructors_without_arguments_as_string = true
  end)

let int () = Random.int 10
let float () = Random.float 1000.
let string () = String.init (Random.int 10) ~f:(fun _ -> Char.to_int 'a' + Random.int 20 |> Char.of_int_exn)
let list ?length f () = let length = Option.value ~default:(Random.int 64) length in List.init length ~f:(fun _ -> f ())
let option f () = match Random.bool () with | true -> Some (f ()) | false -> None

type a = A of int |  B of string | C of float | D of (int * int)
[@@deriving protocol ~driver:(module Json), yojson]
let a () = match Random.int 4 with
  | 0 -> A (int ())
  | 1 -> B (string ())
  | 2 -> C (float ())
  | 3 -> D (int (), int ())
  | _ -> failwith "a"


type b = { a: a; b: a list; c: int }
[@@deriving protocol ~driver:(module Json), yojson]
let b () = {
  a = a ();
  b = list a ();
  c = int () [@default 0];
}

type c = [ `A of string | `B of c * c | `C of int option list]
[@@deriving protocol ~driver:(module Json), yojson]
let rec c () = match Random.int 3 with
  | 0 -> `A (string ())
  | 1 -> `B (c (), c ())
  | 2 -> `C (list (option int) ())
  | _ -> failwith "c"

type d = { a: a list; b: b list; c: c list; }
[@@deriving protocol ~driver:(module Json), yojson]
let d () =
  { a = list a ();
    b = list b ();
    c = list c ();
  }

type e = A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | A9 | A10 | A11 | A12 | A13 | A14 | A15 | A16 | A17 | A18 | A19
[@@deriving protocol ~driver:(module Json), yojson]
let e () = match Random.int 20 with
  | 0 -> A0
  | 1 -> A1
  | 2 -> A2
  | 3 -> A3
  | 4 -> A4
  | 5 -> A5
  | 6 -> A6
  | 7 -> A7
  | 8 -> A8
  | 9 -> A9
  | 10 -> A10
  | 11 -> A11
  | 12 -> A12
  | 13 -> A13
  | 14 -> A14
  | 15 -> A15
  | 16 -> A16
  | 17 -> A17
  | 18 -> A18
  | 19 -> A19
  | _ -> failwith "e"
(* Switch is a jump table: I could enumerate to get an index. But why is the reverse so slow???*)

type f = d * e
[@@deriving protocol ~driver:(module Json), yojson]
let f () = d (), e ()

type t = e list
[@@deriving protocol ~driver:(module Json), yojson]
let t () = list ~length:10 e ()

(** Global data structures *)
let t = t ()
let t' = to_json t
let t'' = of_json t'
let ty' = to_yojson t
let () =
  Yojson.Safe.pretty_to_channel Stdio.stdout t';
  Caml.Printf.printf "\nYojson:\n";
  Yojson.Safe.pretty_to_channel Stdio.stdout ty';
  ()

let () =
  for _ = 0 to 100000 do
    to_json t |> Sys.opaque_identity |> ignore
  done

(*
let () =
  of_json ty' |> ignore;
  of_yojson t' |> ignore;
  ()
*)

let to_json =
  Bench.Test.create ~name:"to_json"
    (fun () -> to_json t |> ignore)

let of_json =
  Bench.Test.create ~name:"of_json"
    (fun () -> of_json t' |> ignore)

let to_yojson =
  Bench.Test.create ~name:"to_yojson"
    (fun () -> to_yojson t |> ignore)

let of_yojson =
  Bench.Test.create ~name:"of_yojson"
    (fun () -> of_yojson ty' |> ignore)

let tests = [ to_json; to_yojson; of_json; of_yojson ]

let command = Bench.make_command tests
let () = Core.Command.run command
