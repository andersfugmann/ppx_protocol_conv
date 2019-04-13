open Protocol_conv_json
type t = [ `A of int [@key "aaa"]| `B of string ]
[@@deriving protocol ~driver:(module Json)]
