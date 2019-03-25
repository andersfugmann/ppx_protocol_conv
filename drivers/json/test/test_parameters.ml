open Protocol_conv_json

module Standard = struct
  type u = A | B of int
  and t = {
    int: int;
    u: u;
    uu: u;
  } [@@deriving protocol ~driver:(module Json)]

  let t = { int = 5; u = A; uu = B 5 }
  let%test _ =  t |> to_json |> of_json = t
  let%expect_test _ =
    let s = Yojson.Safe.pretty_to_string (to_json t) in
    print_endline s;
    [%expect {| { "int": 5, "u": "A", "uu": [ "B", 5 ] } |}]
end

module Field_upper = struct
  module Parameters : Ppx_protocol_driver.Parameters = struct
    let field_name name = String.capitalize_ascii name
    let singleton_constr_as_string = true
    let omit_default_values = true
  end
  module Json = Json.Make(Parameters)
  type u = A | B of int
  and t = {
    int: int;
    u: u;
    uu: u;
  } [@@deriving protocol ~driver:(module Json)]

  let t = { int = 5; u = A; uu = B 5 }
  let%test _ =  t |> to_json |> of_json = t
  let%expect_test _ =
    let s = Yojson.Safe.pretty_to_string (to_json t) in
    print_endline s;
    [%expect {| { "Int": 5, "U": "A", "Uu": [ "B", 5 ] } |}]
end

module Singleton_as_list = struct
  module Parameters : Ppx_protocol_driver.Parameters = struct
    let field_name name = name
    let singleton_constr_as_string = false
    let omit_default_values = true
  end
  module Json = Json.Make(Parameters)
  type u = A | B of int
  and t = {
    int: int;
    u: u;
    uu: u;
  } [@@deriving protocol ~driver:(module Json)]

  let t = { int = 5; u = A; uu = B 5 }
  let%test _ =  t |> to_json |> of_json = t
  let%expect_test _ =
    let s = Yojson.Safe.pretty_to_string (to_json t) in
    print_endline s;
    [%expect {| { "int": 5, "u": [ "A" ], "uu": [ "B", 5 ] } |}]
end

module Omit_default = struct
  type u = A | B of int
  and t = {
    int: int; [@default 5]
    u: u;
    uu: u;
  } [@@deriving protocol ~driver:(module Json)]

  let t = { int = 5; u = A; uu = B 5 }
  let%test _ =  t |> to_json |> of_json = t
  let%expect_test _ =
    let s = Yojson.Safe.pretty_to_string (to_json t) in
    print_endline s;
    [%expect {| { "u": "A", "uu": [ "B", 5 ] } |}]
end

module Keep_default = struct
  module Parameters : Ppx_protocol_driver.Parameters = struct
    let field_name name = name
    let singleton_constr_as_string = true
    let omit_default_values = false
  end
  module Json = Json.Make(Parameters)

  type u = A | B of int
  and t = {
    int: int; [@default 5]
    u: u;
    uu: u;
  } [@@deriving protocol ~driver:(module Json)]

  let t = { int = 5; u = A; uu = B 5 }
  let%test _ =  t |> to_json |> of_json = t
  let%expect_test _ =
    let s = Yojson.Safe.pretty_to_string (to_json t) in
    print_endline s;
    [%expect {| { "int": 5, "u": "A", "uu": [ "B", 5 ] } |}]
end
