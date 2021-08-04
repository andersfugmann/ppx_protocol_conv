open Protocol_conv_json
module Driver = struct
  let name = "json"
  let serialize t = Yojson.Safe.pretty_to_string t
  let deserialize t = Yojson.Safe.from_string t
  include Json
  let of_driver_exn = of_json_exn
  let of_driver = of_json
  let to_driver = to_json
  let sexp_of_t : t -> Sexplib.Sexp.t = fun t ->
    Sexplib.Std.sexp_of_string (to_string_hum t)
end

module Unittest = Test.Unittest.Make(Driver)

module Yojson_Driver = struct
  let name = "yojson"
  let serialize t = Yojson.Safe.pretty_to_string t
  let deserialize t = Yojson.Safe.from_string t
    include Json.Yojson
  let of_driver_exn = of_yojson_exn
  let of_driver = of_yojson
  let to_driver = to_yojson
  let sexp_of_t : t -> Sexplib.Sexp.t = fun t ->
    Sexplib.Std.sexp_of_string (to_string_hum t)
end
module Unittest_yojson = Test.Unittest.Make(Yojson_Driver)

(*
module Identity = struct
  open Sexplib.Std
  type v = {int: a; string: b;}
  [@@deriving protocol ~driver:(module Json), sexp]
  type t = { t: Json.t; }
  [@@deriving protocol ~driver:(module Json), sexp]
  let t = { a = 5; b = "s"} |> to_json in
  let t = { t }
  Alcotest.(check (of_pp fmt)) "Deserialize" t (`Assoc y |> of_json_exn);
  Alcotest.(check (of_pp fmt_yojson)) "Serialize" (to_json t) (`Assoc j);


end
*)
let () = Unittest.run ~extra:[Test_attrib.test] ()
let () = Unittest_yojson.run ~extra:[Test_attrib.test] ()
