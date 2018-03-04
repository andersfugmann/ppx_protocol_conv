module Make : functor (Driver : Testable.Driver) -> sig
  val unittest : printer:(Driver.t -> string) -> OUnit2.test
end
