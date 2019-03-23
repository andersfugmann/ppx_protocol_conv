module Make : functor (Driver : Testable.Driver) -> sig
  val unittest : OUnit2.test
end
