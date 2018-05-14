open OUnit2
module type Driver = Protocol_conv.Runtime.Driver with type 'a flags = 'a

module type Test = functor(Driver: Driver) -> sig
  val unittest : printer:(Driver.t -> string) -> test
end

module Make (Driver: Driver) = struct

  module type Testable = sig
    type t [@@deriving protocol ~driver:(module Driver)]
    val t: t
    val name: string
    val sexp_of_t: t -> Base.Sexp.t
  end

  let test (module T : Testable) =
    let f _ =
      let t' = T.to_driver T.t |> T.of_driver in
      let printer t = Base.Sexp.to_string_hum (T.sexp_of_t t) in
      assert_equal ~printer ~msg:T.name T.t t';
      ()
    in
    T.name >:: f
end
