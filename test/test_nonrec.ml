open OUnit2
module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)

  module Recursive = struct
    type t = Cons of int * t
           | Nil
    [@@deriving protocol ~driver:(module Driver)]

    module Nonrec : M.Testable = struct
      let name = "Nonrec"
      type nonrec t = A of t
      [@@deriving protocol ~driver:(module Driver)]
      let t = A (Cons (4, Cons (3, Nil)))
    end

  end
  let unittest ~printer = __MODULE__ >: test_list [
      M.test (module Recursive.Nonrec) ~printer;
    ]
end
