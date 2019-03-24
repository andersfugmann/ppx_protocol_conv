open OUnit2
open Sexplib.Std

module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)
  type y = A of int | B of string [@@deriving protocol ~driver:(module Driver), sexp]
  type t = {
    int: int; [@default 5]
    string: string; [@default "xyz"]
    list: int list; [@default [1;2;3]]
    key: int; [@default 9] [@key "Key"]
    std: unit;
  } [@@deriving protocol ~driver:(module Driver), sexp]
  let printer t = Base.Sexp.to_string_hum (sexp_of_t t)

  let test_default_unused _ =
    let s =
      Protocol_conv.Runtime.Record_out.(
        ("int", 6, Driver.of_int, None) ^::
        ("string", "abc", Driver.of_string, None) ^::
        ("list", [4;5;6], Driver.of_list Driver.of_int, None) ^::
        ("Key", 100, Driver.of_int, None) ^::
        ("std", (), Driver.of_unit, None) ^::
        Nil
      )
      |> Driver.of_record
    in
    let t = { int = 6;
              string = "abc";
              list = [4;5;6];
              key = 100;
              std = ();
            } in

    assert_equal ~printer t (of_driver s);
    assert_equal s (to_driver t);
    ()

  let test_one_default _ =
    let s =
      Protocol_conv.Runtime.Record_out.(
        ("string", "abc", Driver.of_string, None) ^::
        ("list", [4;5;6], Driver.of_list Driver.of_int, None) ^::
        ("Key", 100, Driver.of_int, None) ^::
        ("std", (), Driver.of_unit, None) ^::
        Nil
      )
      |> Driver.of_record
    in
    let t = { int = 5;
              string = "abc";
              list = [4;5;6];
              key = 100;
              std = ();
            } in

    assert_equal ~printer t (of_driver s);
    assert_equal s (to_driver t);
    ()

  let test_all_default _ =
    let s =
      Protocol_conv.Runtime.Record_out.(
        ("std", (), Driver.of_unit, None) ^:: Nil
      ) |> Driver.of_record
    in
    let t = { int = 5;
              string = "xyz";
              list = [1;2;3];
              key = 9;
              std = ();
            } in
    assert_equal ~printer t (of_driver s);
    assert_equal s (to_driver t);
    ()

  let unittest = __MODULE__ >: test_list [
      "default_unused" >:: test_default_unused;
      "one_default" >:: test_one_default;
      "all_default" >:: test_all_default;
    ]
end
