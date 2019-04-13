open Protocol_conv_json
open Base
open Core_bench.Std

module Json = Json.Make(struct
    include Ppx_protocol_driver.Default_parameters
    let omit_default_values = true
    let strict = true
    let constructors_without_arguments_as_string = true
  end)

module type Test = sig
  val name : string
  type t
  val t: unit -> t
  val to_json: t -> Json.t
  val to_yojson: t -> Yojson.Safe.json
  val of_json: Json.t -> t
  val of_yojson: Yojson.Safe.json -> t Ppx_deriving_yojson_runtime.error_or
end


let int () = Random.int 10
let float () = Random.float 1000.
let string () = String.init (Random.int 10) ~f:(fun _ -> Char.to_int 'a' + Random.int 20 |> Char.of_int_exn)
let list ?length f () = let length = Option.value ~default:(Random.int 64) length in List.init length ~f:(fun _ -> f ())
let option f () = match Random.bool () with | true -> Some (f ()) | false -> None

module Test_tuple = struct
  type t = (int * int * string * int * int)
  [@@deriving protocol ~driver:(module Json), yojson]
  let t () = (int (), int (), string (), int (), int ())
  let name = "Tuple"
end

module Test_variant_record = struct
  let name = "Variant with record"
  type t = A of { a: int; b: int; c: int; d: int; e: int; f: int; }
         | B of { a: int; b: int; c: int; d: int; e: int; f: int; }
         | C of { a: int; b: int; c: int; d: int; e: int; f: int; }
         | D of { a: int; b: int; c: int; d: int; e: int; f: int; }
  [@@deriving protocol ~driver:(module Json), yojson]
  let t () = C {
    a = int ();
    b = int ();
    c = int ();
    d = int ();
    e = int ();
    f = int ();
  }
end

module Test_record : Test = struct
  let name = "Record"
  type t = { a: int; b: int; c: int; d: int; e: int; f: int; }
  [@@deriving protocol ~driver:(module Json), yojson]
  let t () = {
    a = int ();
    b = int ();
    c = int ();
    d = int ();
    e = int ();
    f = int ();
  }
end

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

module Test_enum : Test = struct
  let name = "Enum"
  type t = A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | A9 | A10 | A11 | A12 | A13 | A14 | A15 | A16 | A17 | A18 | A19
  [@@deriving protocol ~driver:(module Json), yojson]
  let t () = match Random.int 20 with
    | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3 | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7 | 8 -> A8 | 9 -> A9 | 10 -> A10
    | 11 -> A11 | 12 -> A12 | 13 -> A13 | 14 -> A14 | 15 -> A15 | 16 -> A16 | 17 -> A17 | 18 -> A18 | 19 -> A19 | _ -> failwith "e"
end

type f = d * e
[@@deriving protocol ~driver:(module Json), yojson]
let f () = d (), e ()

module Test_full : Test = struct
  let name = "Test full"
  type t = f list
  [@@deriving protocol ~driver:(module Json), yojson]
  let t () = list ~length:1 f ()
end


let bench (module X: Test) =
  let t = X.t () in
  let json = X.to_json t in
  let yojson = X.to_yojson t in
  Core.Command.run @@ Bench.make_command @@ [
    Bench.Test.create_group ~name:X.name [
      Bench.Test.create_group ~name:"Deserialize" [
        Bench.Test.create ~name:"to_yojson"
          (fun () -> for _ = 0 to 10000 do X.of_yojson yojson |> ignore done);
        Bench.Test.create ~name:"to_json"
          (fun () -> for _ = 0 to 10000 do X.of_json json |> ignore done);
      ]
    ]
  ];
  Core.Command.run @@ Bench.make_command @@ [
    Bench.Test.create_group ~name:X.name [
      Bench.Test.create_group ~name:"Serialize" [
        Bench.Test.create ~name:"to_yojson"
          (fun () -> for _ = 0 to 10000 do X.to_yojson t |> ignore done);
        Bench.Test.create ~name:"to_json"
          (fun () -> for _ = 0 to 10000 do X.to_json t |> ignore done);
      ];
    ];
  ];
  ()


let () =
  bench (module Test_enum);
  bench (module Test_tuple);
  bench (module Test_variant_record);
  bench (module Test_record);
  bench (module Test_full);
