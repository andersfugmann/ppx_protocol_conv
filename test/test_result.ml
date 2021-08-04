open Sexplib.Std
module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)
  let result_of_sexp = Base.Result.t_of_sexp
  let sexp_of_result = Base.Result.sexp_of_t

  module Result_ok : M.Testable = struct
    let name = "Option.Ok"
    type t = (int, string) result
    [@@deriving protocol ~driver:(module Driver), sexp]
    let t = Ok 2
  end

  module Result_error : M.Testable = struct
    let name = "Option.Error"
    type t = (int, string) result
    [@@deriving protocol ~driver:(module Driver), sexp]
    let t = Error "Error string"
  end

  let unittest = __MODULE__, [
      M.test (module Result_ok);
      M.test (module Result_error);
    ]
end
