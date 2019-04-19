open Sexplib.Std

module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)

  type t = bool option option option
  [@@deriving protocol ~driver:(module Driver), sexp]

  module T1 : M.Testable = struct
    let name = __MODULE__ ^ ".Some Some Some true"
    type nonrec t = t
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = Some (Some (Some true))
  end

  module T2 : M.Testable = struct
    let name = __MODULE__ ^ ".Some Some None"
    type nonrec t = t
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = Some (Some None)
  end

  module T3 : M.Testable = struct
    let name = __MODULE__ ^ ".Some None"
    type nonrec t = t
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = Some None
  end

  module T4 : M.Testable = struct
    let name = __MODULE__ ^ ".None"
    type nonrec t = t
    [@@deriving protocol ~driver:(module Driver), sexp]
    let t = None
  end

  type u = { a: t }
    [@@deriving protocol ~driver:(module Driver), sexp]

  module T5 : M.Testable = struct
    let name = __MODULE__ ^ ".Some Some Some true"
    type t = u
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = { a = Some (Some (Some true)) }
  end

  module T6 : M.Testable = struct
    let name = __MODULE__ ^ ".Some Some None"
    type t = u
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = { a = Some (Some None) }
  end

  module T7 : M.Testable = struct
    let name = __MODULE__ ^ ".Some None"
    type t = u
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = { a = Some None }
  end

  module T8 : M.Testable = struct
    let name = __MODULE__ ^ ".None"
    type t = u
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = { a = None }
  end

  module T9 : M.Testable = struct
    let name = __MODULE__ ^ ".unit option option list option option"
    type t = unit option option list option option
    [@@deriving protocol ~driver:(module Driver), sexp]
    let t = Some (Some ([Some (Some ()); Some None; None]))
  end

  module T10 : M.Testable = struct
    let name = __MODULE__ ^ ".confuse deserialization by using reserved word"
    type v = { option: bool option option }
    [@@deriving protocol ~driver:(module Driver), sexp]
    type t = { o: v }
    [@@deriving protocol ~driver:(module Driver), sexp]
    let t = { o = { option = Some (Some true) } }
  end

  let unittest = __MODULE__, [
      M.test (module T1);
      M.test (module T2);
      M.test (module T3);
      M.test (module T4);
      M.test (module T5);
      M.test (module T6);
      M.test (module T7);
      M.test (module T8);
      M.test (module T9);
      M.test (module T10);
    ]
end
