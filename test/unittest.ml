open OUnit2

let verbose = false
module Make(Driver : Testable.Driver) = struct
   module Unit = Test_unit.Make(Driver)
   module Types = Test_types.Make(Driver)
   module Variant = Test_variant.Make(Driver)
   module Nonrec = Test_nonrec.Make(Driver)
   module Option_unit = Test_option_unit.Make(Driver)
   module Lists = Test_lists.Make(Driver)
   module Arrays = Test_arrays.Make(Driver)
   module Record = Test_record.Make(Driver)
   module Param_types = Test_param_types.Make(Driver)
   module Poly = Test_poly.Make(Driver)
   module TDriver = Test_driver.Make(Driver)
   module Signature = Test_sig.Make(Driver)

  let run ?(extra = []) ~name () =
    let suite = name >::: [
        Unit.unittest;
        Types.unittest;
        Variant.unittest;
        Nonrec.unittest;
        Option_unit.unittest;
        Lists.unittest;
        Arrays.unittest;
        Record.unittest;
        Param_types.unittest;
        Poly.unittest;
        TDriver.unittest;
        Signature.unittest;
      ] @ extra
    in
    open_out "test.out" |> close_out;
    run_test_tt_main suite
end
