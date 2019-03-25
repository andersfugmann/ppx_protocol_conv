(* Json Protocol *)
include Protocol_conv.Runtime.Driver with type t = Yojson.Safe.json [@@warning "-3"]
module Make(P: Ppx_protocol_driver.Parameters) : (Protocol_conv.Runtime.Driver with type t = Yojson.Safe.json) [@@warning "-3"]
module Yojson : sig
  include Protocol_conv.Runtime.Driver with type t = Yojson.Safe.json [@@warning "-3"]
  val of_yojson: t -> t
  val to_yojson: t -> t
end

val of_json: t -> t
val to_json: t -> t
