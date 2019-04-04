type v = A of (int * int ) | B of int * int
[@@deriving to_protocol ~driver:(module Json), sexp]
