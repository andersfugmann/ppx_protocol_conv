open !Base
open Protocol_conv_json
open Protocol_conv_xml
open Protocol_conv_msgpack

module Recursive = struct
  let name = "Rec"
  type t = Cons of int * t
         | Nil
  [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]

  module Nonrec : Util.Testable = struct
    let name = "Nonrec"
    type nonrec t = A of t
    [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light), protocol ~driver:(module Msgpack)]
    let t = A (Cons (4, Cons (3, Nil)))
  end


end
let () = Util.test (module Recursive.Nonrec)
