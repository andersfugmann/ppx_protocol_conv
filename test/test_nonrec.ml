open Sexplib.Std
module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)

  module Recursive = struct
    type t = Cons of int * t
           | Nil
    [@@deriving protocol ~driver:(module Driver), sexp]

    module Nonrec : M.Testable = struct
      let name = __MODULE__ ^ ".Nonrec"
      type nonrec t = A of t
      [@@deriving protocol ~driver:(module Driver), sexp]
      let t = A (Cons (4, Cons (3, Nil)))
    end

  end

  module Recursive2 = struct
    type t = Cons of int * t | Nil
    [@@deriving protocol ~driver:(module Driver), sexp]

    module Nonrec : M.Testable = struct
      let name = __MODULE__ ^ ".Nonrec2"
      type nonrec t = t
      [@@deriving protocol ~driver:(module Driver), sexp]
      let t = Cons (4, Cons (3, Nil))
    end

  end
  let unittest = __MODULE__, [
      M.test (module Recursive.Nonrec);
      M.test (module Recursive2.Nonrec);
    ]
end
