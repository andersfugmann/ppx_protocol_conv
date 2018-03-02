open Protocol_conv_json
open Protocol_conv_xml
open Protocol_conv_msgpack

module SimpleRecord : Util.Testable = struct
  let name = "SimpleRecord"
  type t = {
      code: string [@key "Code"];
      message: string [@key "Message"];
      bucket: string option [@key "Bucket"];
      endpoint: string option [@key "Endpoint"];
      request_id: string [@key "RequestId"];
      host_id: string [@key "HostId"];
    }
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = { code = "Error";
            message = "Message";
            bucket = None;
            endpoint = None;
            request_id = "sdfsd";
            host_id = "SDsd";
          }
end
let () = Util.test (module SimpleRecord)

module RecordList : Util.Testable = struct
  let name = "RecordList"
  type objekt = { key: int }
  and t = { objects : objekt list }
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = { objects = [ { key = 1 }; { key = 2 } ] }
end
let () = Util.test (module RecordList)
