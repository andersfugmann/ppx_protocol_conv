(* Json Protocol *)
include Protocol_conv.Runtime.Driver with type t = Yojson.Safe.t [@@warning "-3"]
module Make(P: Ppx_protocol_driver.Parameters) : (Protocol_conv.Runtime.Driver with type t = Yojson.Safe.t) [@@warning "-3"]
module Yojson : sig
  include Protocol_conv.Runtime.Driver with type t = Yojson.Safe.t [@@warning "-3"]
  val of_yojson_exn: t -> t
  val of_yojson: t -> (t, error) Protocol_conv.Runtime.result
  val to_yojson: t -> t
end

val of_json_exn: t -> t
val of_json: t -> (t, error) Protocol_conv.Runtime.result
val to_json: t -> t
