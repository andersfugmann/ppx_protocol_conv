(*
type a = string * int list [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
type aopt = a option [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
type y = {
  y_a: int;
  y_b: a;
  y_c_: aopt;
} [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
*)
