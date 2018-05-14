open OUnit2
open Sexplib.Std
module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)

  module Recursive = struct
    type t = Cons of int * t
           | Nil
    [@@deriving protocol ~driver:(module Driver), sexp]

    module Nonrec : M.Testable = struct
      let name = "Nonrec"
      type nonrec t = A of t
      [@@deriving protocol ~driver:(module Driver), sexp]
      let t = A (Cons (4, Cons (3, Nil)))
    end

  end
  let unittest = __MODULE__ >: test_list [
      M.test (module Recursive.Nonrec);
    ]
end
