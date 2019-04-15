open OUnit2
open Sexplib.Std
module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)
  module Signature : M.Testable = struct
    module Test_sig : sig
      type ('a, 'b, 'c, 'd, 'e) w = 'a * 'b * 'c * 'd * 'e
      and 'a u = A of ('a, int, int, 'a, int) w | B | C of 'a u
      and 'a v = { x : 'a u }
      and t = [`A of int | `B of float] v
      [@@deriving protocol ~driver:(module Driver), sexp]
    end = struct
      type ('a, 'b, 'c, 'd, 'e) w = 'a * 'b * 'c * 'd * 'e
      and 'a u = A of ('a, int, int, 'a, int) w | B | C of 'a u
      and 'a v = { x : 'a u }
      and t = [`A of int | `B of float] v
      [@@deriving protocol ~driver:(module Driver), sexp]
    end
    let name = __MODULE__ ^ ".Test_sig"

    type t = Test_sig.t
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = { Test_sig.x = A (`A 7, 7, 7, `B 0.7, 7) }
  end

  let unittest = __MODULE__ >: test_list [
      M.test (module Signature);
    ]
end
