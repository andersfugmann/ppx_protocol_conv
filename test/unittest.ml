open OUnit2

let verbose = false

module Make(Driver : Testable.Driver) = struct

  let run ~printer =
    let module Types = Test_types.Make(Driver) in
    let module Variant = Test_variant.Make(Driver) in
    let module Nonrec = Test_nonrec.Make(Driver) in
    let module Lists = Test_lists.Make(Driver) in
    let module Record = Test_record.Make(Driver) in
    let module Driver = Test_driver.Make(Driver) in

    let suite = "unittest" >::: [
        Types.unittest ~printer;
        Variant.unittest ~printer;
        Nonrec.unittest ~printer;
        Lists.unittest ~printer;
        Record.unittest ~printer;
        Driver.unittest ~printer;
    ]
    in
  run_test_tt_main suite
end
