open Protocol_conv_json
open Protocol_conv_xml
open Protocol_conv_msgpack

module S3 : Util.Testable = struct
  let name = "S3"
  type storage_class = Standard [@key "STANDARD"]
                     | Standard_ia [@key "STANDARD_IA"]
                     | Reduced_redundancy [@key "REDUCED_REDUNDANCY"]
                     | Glacier [@key "GLACIER"]


  and content = {
    storage_class: storage_class [@key "StorageClass"];
    etag: string [@key "ETag"];
  }
  and result = {
    prefix: string option [@key "Prefix"];
    contents: content list [@key "Contents"];
  }
  and t = result
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = { prefix = Some "prefix";
            contents = [ { storage_class = Standard; etag = "Etag" } ]
          }
end
let () = Util.test (module S3)



module EmptyList : Util.Testable = struct
  let name = "SingleElem"
  type t = int list
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = []
end
let () = Util.test (module EmptyList)

module Singleton : Util.Testable = struct
  let name = "SingleElem"
  type t = int list
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = [2]
end
let () = Util.test (module Singleton)

module LongList : Util.Testable = struct
  let name = "Longlist"
  type t = int list
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = [4; 2; 3; 1]
end
let () = Util.test (module LongList)

module EmptyInsideRec : Util.Testable = struct
  let name = "EmptyInsideRec"
  type v = int [@key "A"]
  and t = { a : string;
            b : v list; [@key "V"]
            c : string;
          }
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = { a= "a"; b = []; c = "c" }
end
let () = Util.test (module EmptyInsideRec)


module SingleInsideRec : Util.Testable = struct
  let name = "SingleInsideRec"
  type v = int [@key "A"]
  and t = { a : string;
            b : v list; [@key "V"]
            c : string;
          }
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = { a= "a"; b = [2]; c = "c" }
end
let () = Util.test (module SingleInsideRec)


module MultiInsideRec : Util.Testable = struct
  let name = "MultiInsideRec"
  type v = int [@key "A"]
  and t = { a : string;
            b : v list; [@key "V"]
            c : string;
          }
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = { a= "a"; b = [4; 2; 3; 1]; c = "c" }
end
let () = Util.test (module SingleInsideRec)

module ListOfLists : Util.Testable = struct
  let name = "ListOfLists"
  type v = int list
  and t = { a : v list; }
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = { a = [ [2;3]; [4;5] ] }
end
let () = Util.test (module ListOfLists)

module ListOfLists2 : Util.Testable = struct
  let name = "ListOfLists2"
  type t = int list list list
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  let t = [ []; [ []; [2]; [3;4]; ]; [ [] ]; [ [2] ]; ]
end
let () = Util.test (module ListOfLists)
