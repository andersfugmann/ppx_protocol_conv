(* Test various types *)
open Protocol_conv_json

module Int = struct
  type t = int [@@deriving protocol ~driver:(module Json)]
  let v = 3
  let () = Util.test_json "int" t_to_json t_of_json v
end

module Option = struct
  type t = (int * string) option [@@deriving protocol ~driver:(module Json)]
  let v = Some (3, "string")
  let () = Util.test_json "int option" t_to_json t_of_json v
end

module String = struct
  type t = string [@@deriving protocol ~driver:(module Json)]
  let v = "string"
  let () = Util.test_json "string" t_to_json t_of_json v
end

module Record = struct
  type t = { a: String.t;
             b: Int.t } [@@deriving protocol ~driver:(module Json)]
  let v = { a=String.v; b=Int.v }
  let () = Util.test_json "record" t_to_json t_of_json v
end

module Lazy = struct
  type t = int lazy_t  [@@deriving protocol ~driver:(module Json)]
  let v = Lazy.from_val 5
  let () =
    match v, t_to_json v |> t_of_json with
    | lazy n, lazy m -> assert (n = m)
end
