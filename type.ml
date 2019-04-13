open Protocol_conv_json
type t = A of { a: int; b: int; c: int; d: int; e: int; f: int; }
       | B of { a: int; b: int; c: int; d: int; e: int; f: int; }
       | C of { a: int; b: int; c: int; d: int; e: int; f: int; }
       | D of { a: int; b: int; c: int; d: int; e: int; f: int; }
[@@deriving protocol ~driver:(module Json)]
