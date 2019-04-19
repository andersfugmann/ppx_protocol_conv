open Sexplib.Std
module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)
  module T1 : M.Testable = struct
    let name = "simple"
    type 'a v = 'a
    [@@deriving protocol ~driver:(module Driver), sexp]
    type t = int v
    [@@deriving protocol ~driver:(module Driver), sexp]
    let t = 5
  end

  module T2 : M.Testable = struct
    let name = "record"
    type 'a v = { a : 'a }
    [@@deriving protocol ~driver:(module Driver), sexp]
    type t = int v
    [@@deriving protocol ~driver:(module Driver), sexp]
    let t = { a = 5 }
  end

  module T3 : M.Testable = struct
    let name = "multiple"
    type ('a, 'b, 'c) v = ('a * 'b * 'c)
    [@@deriving protocol ~driver:(module Driver), sexp]

    type t = (int, string, bool) v
    [@@deriving protocol ~driver:(module Driver), sexp]
    let t = (5, "5", true)
  end

  module T4_1 = struct
    type 'a t = { a: 'a }
    [@@deriving protocol ~driver:(module Driver), sexp]
  end

  module T4 : M.Testable = struct
    let name = "reference"
    type 'a v = { a: 'a T4_1.t }
    [@@deriving protocol ~driver:(module Driver), sexp]
    type t = int v
    [@@deriving protocol ~driver:(module Driver), sexp]
    let t = { a = { T4_1.a = 5} }
  end

  module T5 : M.Testable = struct
    let name = "recursive"
    type 'a v = { a: 'a }
    [@@deriving protocol ~driver:(module Driver), sexp]
    type 'a u = { b: 'a }
    [@@deriving protocol ~driver:(module Driver), sexp]
    type 'a w = { c: 'a }
    [@@deriving protocol ~driver:(module Driver), sexp]

    type t = ((int v) u) w
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = { c = { b = { a = 5 }}}
  end

  let unittest = __MODULE__, [
      M.test (module T1);
      M.test (module T2);
      M.test (module T3);
      M.test (module T4);
      M.test (module T5);
    ]

end
