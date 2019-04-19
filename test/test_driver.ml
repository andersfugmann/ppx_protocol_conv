open Base

module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)
  module Tuple : M.Testable = struct
    let name = "Tuple"

    type t = (int * int list * string list * (int * int) list) list
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = [
      (10, [20;30;40], ["s50"; "s60"; "s70"], [100, 200; 300, 400; 500, 600]);
      (11, [21;31;41], ["s51"; "s61"; "s71"], [101, 201; 301, 401; 501, 601]);
      (12, [22;32;42], ["s52"; "s62"; "s72"], [102, 202; 302, 402; 502, 602]);
      (13, [23;33;43], ["s53"; "s63"; "s73"], [103, 203; 303, 403; 503, 603]);
    ]
  end

  module Any : M.Testable = struct
    let name = "Any"

    type t1 = { x: int; y: string }
    and v = A | B of int list * int list | C of string
    and t = {
      bool: bool;
      char: char;
      int: int;
      int32: int32;
      int64: int64;
      nativeint: nativeint;
      float: float;
      string: string;
      intlist: int list;
      intoption: int option;
      intref: int ref;
      tuple: (int * string * bool);
      vlist: v list;
      varray: v array;
      record: t1;
      mutable z: int;
    }
    [@@deriving protocol ~driver:(module Driver), sexp]

    let v = B ([5; 6; 7], [10;11;12])

    let t = {
      bool = true;
      char = 'x';
      int = 2;
      int32 = Int32.of_int_exn 5;
      int64 = Int64.of_int_exn 10;
      nativeint = Nativeint.of_int 20;
      float = 3.14;
      string = "string";
      intlist = [3; 4; 5];
      intoption = Some 100;
      intref = ref 4;
      tuple = (5, "protocol", false);
      vlist = [ v; v; v; ];
      varray = [| v; v; v; |];
      record = { x = 5; y = "string" };
      z = 101;
    }
  end

  module Record : M.Testable = struct
    let name = "Record"
    type a = {
      a_int: int;
      a_string: string;
    }
    and b = {
      b_int: int;
      b_string: string;
      b_a: a;
      b_al: a list;
    }
    and t = {
      t_a: a;
      t_al: a list;
      t_b: b;
      t_bl: b list;
      t_i: int;
      t_t: (int * int * string list);
      t_tl: (int * int * string list) list;
      t_il: int list;
    }
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = {
      t_a = { a_int = 1; a_string = "s1"; };
      t_al = [
        { a_int = 2; a_string = "s2"; };
        { a_int = 3; a_string = "s3"; };
        { a_int = 4; a_string = "s4"; };
      ];
      t_b = { b_int = 5;
              b_string = "s5";
              b_a = { a_int = 6; a_string = "s6"; };
              b_al = [
                { a_int = 7; a_string = "s7"; };
                { a_int = 8; a_string = "s8"; };
                { a_int = 9; a_string = "s9"; };
              ];
            };
      t_bl = [];
      t_i = 1000;
      t_t = (100, 101, ["s100"; "s101"]);
      t_tl = [
        (100, 101, ["s100"; "s101"]);
        (110, 111, ["s110"; "s111"]);
        (120, 121, ["s120"; "s121"]);
        (130, 131, ["s130"; "s131"]);
        (140, 141, ["s140"; "s141"]);
      ];
      t_il = [1000; 1001; 1002]
    }
  end

  module List : M.Testable = struct
    let name = "list"
    type t = { a: int list }
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = { a = [1; 2; 3] }

  end

  module Array : M.Testable = struct
    let name = "array"
    type t = { a: int array }
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = { a = [|1; 2; 3|] }

  end


  module Lists : M.Testable = struct
    let name = "Lists"

    type l = A of int list | B of int list list * int list * int | C of int list * int list
    and t = {
      a: int list list;
      b: (int list * int list);
      c: int list;
      l: l list;
    }
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = {
      a = [
        [ 1;2;3 ];
        [];
        [10; 20; 30; 40];
        [100; 101];
      ];

      b = ([8;9], [10;20;30;40]);
      c = [100; 101; 102; 103];

      l = [
        A [1;2;3];
        B ([[1;2]; [3;4;5]; [2]], [3;1], 5);
        C ([1;2;3], [3;4;5]);
      ]
    }
  end
  let unittest = Caml.__MODULE__, [
      M.test (module Tuple);
      M.test (module Any);
      M.test (module Record);
      M.test (module List);
      M.test (module Lists);
      M.test (module Array);
    ]

end
