module Make : functor (_ : Testable.Driver) -> sig
  val unittest : unit Alcotest.test
end
