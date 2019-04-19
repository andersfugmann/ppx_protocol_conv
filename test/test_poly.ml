open Sexplib.Std
module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)

  module Simple : M.Testable = struct
    let name = __MODULE__ ^ ".Simple"
    type v = [ `A | `B of int | `C of int * int | `D of (int * int) ]
    and t = v list
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = [ `A; `B 5; `C (6,7); `D (8,9) ]
  end

  module Tree : M.Testable = struct
    let name = __MODULE__ ^ ".Tree"
    type t = [ `Node of t * int * t | `Leaf ]
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = `Node ( `Node (`Leaf, 3, `Leaf), 10, `Leaf)
  end

  module MutualRecursion : M.Testable = struct
    let name = __MODULE__ ^ ".MutualRecursion"
    type v = [ `V1 of v | `V0 of int | `T of t ]
    and t = [ `T1 of t | `T2 of int | `V of v ]
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = `T1 (`V (`T (`V (`V1 (`V1 (`V1 (`V0 5)))))))
  end

  module InsideRec : M.Testable = struct
    let name = __MODULE__ ^ ".InsideRec"
    type v = [ `V0 [@key "A"]
             | `V1 [@key "B"] ]

    and t = { a : string;
              b : v; [@key "V"]
              c : string;
            }
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = { a= "a"; b = `V0; c = "c" }
  end

  let unittest = __MODULE__, [
      M.test (module Simple);
      M.test (module Tree);
      M.test (module MutualRecursion);
      M.test (module InsideRec);
    ]
end
