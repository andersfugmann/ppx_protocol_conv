open Sexplib.Std
module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)
  type t = unit option option option option
  [@@deriving protocol ~driver:(module Driver), sexp]
  module None : M.Testable = struct
    type nonrec t = t [@@deriving protocol ~driver:(module Driver), sexp]
    let name = "None"
    let t = None
  end
  module Some_none : M.Testable = struct
    type nonrec t = t [@@deriving protocol ~driver:(module Driver), sexp]
    let name = "Some None"
    let t = Some None
  end
  module Some_some_none: M.Testable = struct
    type nonrec t = t [@@deriving protocol ~driver:(module Driver), sexp]
    let name = "Some Some None"
    let t = Some (Some None)
  end
  module Some_some_some_none : M.Testable = struct
    type nonrec t = t [@@deriving protocol ~driver:(module Driver), sexp]
    let name = "Some Some Some None"
    let t = Some (Some (Some None))
  end
  module Some_some_some_some_unit : M.Testable = struct
    type nonrec t = t [@@deriving protocol ~driver:(module Driver), sexp]
    let name = "Some Some Some Unit"
    let t = Some (Some (Some (Some ())))
  end
  let unittest = __MODULE__, [
      M.test (module None);
      M.test (module Some_none);
      M.test (module Some_some_none);
      M.test (module Some_some_some_some_unit);
    ]
end
