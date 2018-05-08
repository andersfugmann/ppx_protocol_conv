open OUnit2

module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)

    type t = {
      unit: unit;
      unit_option: unit option;
      unit_option_option: unit option option;
      bool: bool;
      bool_option: bool option;
      bool_option_option: bool option option;
    }
      [@@deriving protocol ~driver:(module Driver)]

    module T1 : M.Testable = struct
      let name = "Unit1"

      type nonrec t = t
      [@@deriving protocol ~driver:(module Driver)]
      let t =
        { unit = (); unit_option = None; unit_option_option = None;
          bool = false;
          bool_option = None; bool_option_option = None }
    end

    module T2 : M.Testable = struct
      let name = "Unit2"

      type nonrec t = t
      [@@deriving protocol ~driver:(module Driver)]
      let t =
        { unit = (); unit_option = Some (); unit_option_option = Some None;
          bool = true;
          bool_option = Some false; bool_option_option = None }
    end

    module T3 : M.Testable = struct
      let name = "Unit3"

      type nonrec t = t
      [@@deriving protocol ~driver:(module Driver)]
      let t =
        { unit = (); unit_option = Some (); unit_option_option = Some (Some ());
          bool = true;
          bool_option = Some true; bool_option_option = Some (Some false) }
    end

    module T4 : M.Testable = struct
      let name = "Unit4"

      type nonrec t = t
      [@@deriving protocol ~driver:(module Driver)]
      let t =
        { unit = (); unit_option = Some (); unit_option_option = Some (Some ());
          bool = true;
          bool_option = Some true; bool_option_option = Some (Some true) }
    end
  let unittest ~printer = __MODULE__ >: test_list [
      M.test (module T1) ~printer;
      (* M.test (module T2) ~printer; *)
      (* M.test (module T3) ~printer; *)
      (* M.test (module T4) ~printer; *)
    ]
end
