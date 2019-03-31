module Driver : Ppx_protocol_driver.Driver with type t = Yojson.Safe.json [@warning "-3"] = struct
  type t = Yojson.Safe.json [@warning "-3"]
  module U = Yojson.Safe.Util

  let to_string_hum t =
    Yojson.Safe.pretty_to_string t

  let of_list l = `List l
  let to_list = U.to_list
  let is_list = function `List _ -> true | _ -> false

  let of_array l = `List (Array.to_list l)
  let to_array t = U.to_list t |> Array.of_list

  let of_alist a = `Assoc a
  let to_alist = U.to_assoc
  let is_alist = function `Assoc _ -> true | _ -> false

  let of_int i = `Int i
  let to_int = U.to_int

  let of_int32 i = Int32.to_int i |> of_int
  let to_int32 t = to_int t |> Int32.of_int

  let of_int64 i = Int64.to_int i |> of_int
  let to_int64 t = to_int t |> Int64.of_int

  let of_float f = `Float f
  let to_float t = U.to_float t

  let of_string s = `String s
  let to_string = U.to_string
  let is_string = function `String _ -> true | _ -> false

  let of_char c = of_string (String.make 1 c)
  let to_char t = match to_string t with
    | s when String.length s = 1 -> s.[0]
    | _ -> failwith "Got string with length != 1 when reading type 'char'"

  let of_bool b = `Bool b
  let to_bool = U.to_bool

  let null = `Null
  let is_null = function `Null -> true | _ -> false
end

include Ppx_protocol_driver.Make(Driver)(Ppx_protocol_driver.Default_parameters)
module Make(P: Ppx_protocol_driver.Parameters) = Ppx_protocol_driver.Make(Driver)(P)

module Y = Yojson.Safe
module Yojson = struct
  include Make(struct
      let omit_default_values = true
      let field_name name = name
      let variant_name name = name
      let singleton_constr_as_string = false
      let eager = true
    end)
  let to_yojson t = t
  let of_yojson t = t
end



(* Allow referencing Json.t in structures. *)
let of_json t = t
let to_json t = t

(** Test parameters. *)
module Test = struct
  module Standard = struct
    module Json = Make(Ppx_protocol_driver.Default_parameters)
    type u = A | B of int
    and t = {
      int: int;
      u: u;
      uu: u;
    } [@@deriving protocol ~driver:(module Json)]
    let t = { int = 5; u = A; uu = B 5 }
    let%test _ =  t |> to_json |> of_json = t
    let%expect_test _ =
      let s = Json.to_string_hum (to_json t) in
      print_endline s;
      [%expect {| { "uu": [ "B", 5 ], "u": "A", "int": 5 } |}]
  end

  module Field_upper = struct
    module Parameters : Ppx_protocol_driver.Parameters = struct
      let field_name name = String.capitalize_ascii name
      let variant_name name = name
      let singleton_constr_as_string = true
      let omit_default_values = true
      let eager = true
    end
    module Json = Make(Parameters)
    type u = A | B of int
    and t = {
      int: int;
      u: u;
      uu: u;
    } [@@deriving protocol ~driver:(module Json)]

    let t = { int = 5; u = A; uu = B 5 }
    let%test _ =  t |> to_json |> of_json = t
    let%expect_test _ =
      let s = Json.to_string_hum (to_json t) in
      print_endline s;
      [%expect {| { "Uu": [ "B", 5 ], "U": "A", "Int": 5 } |}]
  end

  module Singleton_as_list = struct
    module Parameters : Ppx_protocol_driver.Parameters = struct
      let field_name name = name
      let variant_name name = name
      let singleton_constr_as_string = false
      let omit_default_values = true
      let eager = true
    end
    module Json = Make(Parameters)
    type u = A | B of int
    and t = {
      int: int;
      u: u;
      uu: u;
    } [@@deriving protocol ~driver:(module Json)]

    let t = { int = 5; u = A; uu = B 5 }
    let%test _ =  t |> to_json |> of_json = t
    let%expect_test _ =
      let s = Json.to_string_hum (to_json t) in
      print_endline s;
      [%expect {| { "uu": [ "B", 5 ], "u": [ "A" ], "int": 5 } |}]
  end

  module Omit_default = struct
    module Parameters : Ppx_protocol_driver.Parameters = struct
      let field_name name = name
      let variant_name name = name
      let singleton_constr_as_string = true
      let omit_default_values = true
      let eager = true
    end
    module Json = Make(Parameters)
    type u = A | B of int
    and t = {
      int: int; [@default 5]
      u: u;
      uu: u;
    } [@@deriving protocol ~driver:(module Json)]

    let t = { int = 5; u = A; uu = B 5 }
    let%test _ =  t |> to_json |> of_json = t
    let%expect_test _ =
      let s = Json.to_string_hum (to_json t) in
      print_endline s;
      [%expect {| { "uu": [ "B", 5 ], "u": "A" } |}]
  end

  module Keep_default = struct
    module Parameters : Ppx_protocol_driver.Parameters = struct
      let field_name name = name
      let variant_name name = name
      let singleton_constr_as_string = true
      let omit_default_values = false
      let eager = true
    end
    module Json = Make(Parameters)

    type u = A | B of int
    and t = {
      int: int; [@default 5]
      u: u;
      uu: u;
    } [@@deriving protocol ~driver:(module Json)]

    let t = { int = 5; u = A; uu = B 5 }
    let%test _ =  t |> to_json |> of_json = t
    let%expect_test _ =
      let s = Json.to_string_hum (to_json t) in
      print_endline s;
      [%expect {| { "uu": [ "B", 5 ], "u": "A" } |}]
  end

  module Field_name_count = struct
    let count = ref 0
    module Parameters : Ppx_protocol_driver.Parameters = struct
      let field_name name = incr count; name
      let variant_name name = name
      let singleton_constr_as_string = true
      let omit_default_values = true
      let eager = true
    end
    module Json = Make(Parameters)
    type u = A of { x:int; y: int; z:int } | B
    [@@deriving protocol ~driver:(module Json)]
    type t = {
      a: int;
      b: int;
      c: u;
    }
    [@@deriving protocol ~driver:(module Json)]

    let expect = !count
    let t = { a=5; b=5; c=B }
    let%test _ =  t |> to_json |> of_json = t
    let%test _ =  (expect = !count) || true
    let%expect_test _ =
      let c1 = !count in
      let _ = t |> to_json |> of_json in
      let c2 = !count in
      let _ = t |> to_json |> of_json in
      let c3 = !count in
      Printf.printf "%d -> %d -> %d -> %d" expect c1 c2 c3;
      [%expect {| 12 -> 12 -> 12 -> 12 |}]
  end
end
