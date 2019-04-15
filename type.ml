open Protocol_conv_json

module Test_sig : sig
  type ('a, 'b) t = 'a * 'b
  and 'a u = A of ('a, int) t | B | C of 'a u
  and 'a v = { x : 'a u }
  and w = [`A of int | `B of float] v
  [@@deriving protocol ~driver:(module Json), sexp]
end = struct
  type ('a, 'b) t = 'a * 'b
  and 'a u = A of ('a, int) t | B | C of 'a u
  and 'a v = { x : 'a u }
  and w = [`A of int | `B of float] v
  [@@deriving protocol ~driver:(module Json)]
end
