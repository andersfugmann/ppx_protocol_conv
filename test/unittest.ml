module type Test_module = sig
  module Make : functor (Driver : Testable.Driver) -> sig
    val unittest: unit Alcotest.test
  end
end

let verbose = false
module Make(Driver : Testable.Driver) = struct
  let test_modules : (module Test_module) list =
    [
      (module Test_arrays);
      (module Test_driver);
      (module Test_lists);
      (module Test_nonrec);
      (module Test_option_unit);
      (module Test_param_types);
      (module Test_poly);
      (module Test_record);
      (module Test_sig);
      (module Test_types);
      (module Test_unit);
      (module Test_variant);
    ]

  (* Create a list of tests *)
  let run ?(extra = []) ~name () =
    let tests =
      List.map (fun (module Test : Test_module) ->
          let module T = Test.Make(Driver) in
          T.unittest)
        test_modules
    in
    let tests = tests @ extra in
    open_out "unittest.output" |> close_out;
    Alcotest.run name tests
end
