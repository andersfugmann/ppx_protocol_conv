open Sexplib.Std
module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)
  module EmptyArray : M.Testable = struct
    let name = __MODULE__ ^ ".SingleElem"
    type t = int array
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = [||]
  end

  module Singleton : M.Testable = struct
    let name = __MODULE__ ^ ".SingleElem"
    type t = int array
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = [|2|]
  end

  module LongArray : M.Testable = struct
    let name = __MODULE__ ^ ".Longarray"
    type t = int array
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = [|4; 2; 3; 1|]
  end

  module EmptyInsideRec : M.Testable = struct
    let name = __MODULE__ ^ ".EmptyInsideRec"
    type v = int [@key "A"]
    and t = { a : string;
              b : v array; [@key "V"]
              c : string;
            }
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = { a= "a"; b = [||]; c = "c" }
  end

  module SingleInsideRec : M.Testable = struct
    let name = __MODULE__ ^ ".SingleInsideRec"
    type v = int [@key "A"]
    and t = { a : string;
              b : v array; [@key "V"]
                c : string;
            }
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = { a= "a"; b = [|2|]; c = "c" }
  end

  module MultiInsideRec : M.Testable = struct
    let name = __MODULE__ ^ ".MultiInsideRec"
    type v = int [@key "A"]
    and t = { a : string;
              b : v array; [@key "V"]
                c : string;
            }
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = { a= "a"; b = [|4; 2; 3; 1|]; c = "c" }
  end

  module ArrayOfArrays : M.Testable = struct
    let name = __MODULE__ ^ ".ArrayOfArrays"
    type v = int array
    and t = { a : v array; }
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = { a = [| [|2;3|]; [|4;5|] |] }
  end

  module ArrayOfArrays2 : M.Testable = struct
    let name = __MODULE__ ^ ".ArrayOfArrays2"
    type t = int array array array
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = [| [||]; [| [||]; [|2|]; [|3;4|]; |]; [| [||] |]; [| [|2|] |]; |]
  end

  let unittest = __MODULE__, [
      M.test (module EmptyArray);
      M.test (module Singleton);
      M.test (module LongArray);
      M.test (module EmptyInsideRec);
      M.test (module SingleInsideRec);
      M.test (module MultiInsideRec);
      M.test (module ArrayOfArrays);
      M.test (module ArrayOfArrays2);
    ]
end
