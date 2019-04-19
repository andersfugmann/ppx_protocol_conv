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

  module Signature2 : M.Testable = struct
    module Test_sig : sig
      type ('a, 'b, 'c, 'd, 'e, 'f, 'g) w = 'a * 'b * 'c * 'd * 'e * 'f * 'g
      and a = int
      and b = float
      and c = string
      and d = unit
      and ('a, 'b, 'c) e = A of 'a | B of 'b | C of 'c
      and ('a, 'b, 'c) f = { a: 'a; b: 'b; c: 'c}
      and ('a, 'b, 'c) g = [ `A of 'a | `B of 'b | `C of 'c]
      and t = (a, b, c, d, (a, b, c) e, (a, b, c) f, (a, b, c) g) w
      [@@deriving protocol ~driver:(module Driver), sexp]
    end = struct
      type ('a, 'b, 'c, 'd, 'e, 'f, 'g) w = 'a * 'b * 'c * 'd * 'e * 'f * 'g
      and a = int
      and b = float
      and c = string
      and d = unit
      and ('a, 'b, 'c) e = A of 'a | B of 'b | C of 'c
      and ('a, 'b, 'c) f = { a: 'a; b: 'b; c: 'c}
      and ('a, 'b, 'c) g = [ `A of 'a | `B of 'b | `C of 'c]
      and t = (a, b, c, d, (a, b, c) e, (a, b, c) f, (a, b, c) g) w
      [@@deriving protocol ~driver:(module Driver), sexp]
    end
    let name = __MODULE__ ^ ".Test_sig2"
    type t = Test_sig.t
    [@@deriving protocol ~driver:(module Driver), sexp]
    let a = 1
    let b = 2.0
    let c = "3.0"
    let d = ()
    let e = Test_sig.A a
    let f = { Test_sig.a; b; c}
    let g = `A a

    let t = (a, b, c, d, e, f, g)
  end

  let unittest = __MODULE__, [
      M.test (module Signature);
      M.test (module Signature2);
    ]
end
