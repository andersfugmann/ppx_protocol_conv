open OUnit2
open Sexplib.Std
module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)

  module SimpleRecord : M.Testable = struct
    let name = "SimpleRecord"
    type t = {
      code: string [@key "Code"];
      message: string [@key "Message"];
      bucket: string option [@key "Bucket"];
      endpoint: string option [@key "Endpoint"];
      request_id: string [@key "RequestId"];
      host_id: string [@key "HostId"];
    }
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = { code = "Error";
              message = "Message";
              bucket = None;
              endpoint = None;
              request_id = "sdfsd";
              host_id = "SDsd";
            }
  end

  module RecordList : M.Testable = struct
    let name = "RecordList"

    type objekt = { key: int }
    and t = { objects : objekt list }
    [@@deriving protocol ~driver:(module Driver), sexp]

    let t = { objects = [ { key = 1 }; { key = 2 } ] }
  end

  let unittest = __MODULE__ >: test_list [
      M.test (module RecordList);
      M.test (module SimpleRecord);
    ]
end
