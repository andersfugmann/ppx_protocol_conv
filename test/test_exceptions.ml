open Sexplib.Std

let list_init ~len ~f =
  let rec inner acc = function
    | 0 -> acc
    | n -> inner (f (n-1) :: acc) (n - 1)
  in
  inner [] len

module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)

  module Stack_overflow = struct
    type t = int list
    [@@deriving protocol ~driver:(module Driver), sexp]
    let t = list_init ~len:1_000_000 ~f:(fun i -> i)
    let name = "stack_overflow"
    let test =
      Alcotest.test_case name `Quick (fun () ->
          try
            to_driver t
            |> of_driver_exn
            |> ignore
          with
          | Failure "ignore" [@warning "-52"] -> ()
        )
  end

  module Exceptions = struct
    type t = { text: string }
    [@@deriving protocol ~driver:(module Driver), sexp]
    type u = string
    [@@deriving protocol ~driver:(module Driver), sexp]
    let t' = u_to_driver "test string"

    (* This should raise an exception *)
    let test =
      Alcotest.test_case "Test exception handling" `Quick (fun () ->
          of_driver t' |> ignore
    )
  end

  let unittest = __MODULE__, [
      Stack_overflow.test;
      Exceptions.test
    ]
end
