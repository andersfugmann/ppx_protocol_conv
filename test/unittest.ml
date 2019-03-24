open OUnit2

let verbose = false

module Make(Driver : Testable.Driver) = struct

  let run ?(extra = []) ~name () =
    let module Unit = Test_unit.Make(Driver) in
    let module Types = Test_types.Make(Driver) in
    let module Variant = Test_variant.Make(Driver) in
    let module Nonrec = Test_nonrec.Make(Driver) in
    let module Option_unit = Test_option_unit.Make(Driver) in
    let module Lists = Test_lists.Make(Driver) in
    let module Arrays = Test_arrays.Make(Driver) in
    let module Record = Test_record.Make(Driver) in
    let module Default = Test_default.Make(Driver) in
    let module Param_types = Test_param_types.Make(Driver) in
    let module Poly = Test_poly.Make(Driver) in
    let module Driver = Test_driver.Make(Driver) in

    let suite = name >::: [
        Unit.unittest;
        Types.unittest;
        Variant.unittest;
        Nonrec.unittest;
        Option_unit.unittest;
        Lists.unittest;
        Arrays.unittest;
        Record.unittest;
        Default.unittest;
        Param_types.unittest;
        Poly.unittest;
        Driver.unittest;
      ] @ extra
    in
  run_test_tt_main suite
end
