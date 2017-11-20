open !Base
open !Protocol_conv_json
open !Protocol_conv_xml

exception Default

module Both : sig
  type t = int [@@deriving protocol ~driver:(module Json)]
end

module Serialize : sig
  type t = int [@@deriving to_protocol ~driver:(module Json)]
end


module Deserialize : sig
  type t = int [@@deriving of_protocol ~driver:(module Json)]
end
