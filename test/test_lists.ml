open OUnit2
module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)

  module EmptyList : M.Testable = struct
    let name = "SingleElem"
    type t = int list
    [@@deriving protocol ~driver:(module Driver)]

    let t = []
  end

  module Singleton : M.Testable = struct
    let name = "SingleElem"
    type t = int list
    [@@deriving protocol ~driver:(module Driver)]

    let t = [2]
  end

  module LongList : M.Testable = struct
    let name = "Longlist"
    type t = int list
    [@@deriving protocol ~driver:(module Driver)]

    let t = [4; 2; 3; 1]
  end

  module EmptyInsideRec : M.Testable = struct
    let name = "EmptyInsideRec"
    type v = int [@key "A"]
    and t = { a : string;
              b : v list; [@key "V"]
                c : string;
            }
    [@@deriving protocol ~driver:(module Driver)]

    let t = { a= "a"; b = []; c = "c" }
  end

  module SingleInsideRec : M.Testable = struct
    let name = "SingleInsideRec"
    type v = int [@key "A"]
    and t = { a : string;
              b : v list; [@key "V"]
                c : string;
            }
    [@@deriving protocol ~driver:(module Driver)]

    let t = { a= "a"; b = [2]; c = "c" }
  end

  module MultiInsideRec : M.Testable = struct
    let name = "MultiInsideRec"
    type v = int [@key "A"]
    and t = { a : string;
              b : v list; [@key "V"]
                c : string;
            }
    [@@deriving protocol ~driver:(module Driver)]

    let t = { a= "a"; b = [4; 2; 3; 1]; c = "c" }
  end

  module ListOfLists : M.Testable = struct
    let name = "ListOfLists"
    type v = int list
    and t = { a : v list; }
    [@@deriving protocol ~driver:(module Driver)]

    let t = { a = [ [2;3]; [4;5] ] }
  end

  module ListOfLists2 : M.Testable = struct
    let name = "ListOfLists2"
    type t = int list list list
    [@@deriving protocol ~driver:(module Driver)]

    let t = [ []; [ []; [2]; [3;4]; ]; [ [] ]; [ [2] ]; ]
  end

  let unittest ~printer = __MODULE__ >: test_list [
      M.test (module EmptyList) ~printer;
      M.test (module Singleton) ~printer;
      M.test (module LongList) ~printer;
      M.test (module EmptyInsideRec) ~printer;
      M.test (module SingleInsideRec) ~printer;
      M.test (module MultiInsideRec) ~printer;
      M.test (module ListOfLists) ~printer;
      M.test (module ListOfLists2) ~printer;
    ]
end
