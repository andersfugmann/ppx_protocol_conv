module Make(P: Ppx_protocol_driver.Parameters) : (Protocol_conv.Runtime.Driver with type t = Msgpck.t)
include Protocol_conv.Runtime.Driver with type t = Msgpck.t

val of_msgpack_exn: t -> t
val to_msgpack: t -> t

type bytes = string
val bytes_of_msgpack_exn: t -> bytes
val bytes_to_msgpack: bytes -> t

type uint32 = int
val uint32_of_msgpack_exn: t -> uint32
val uint32_to_msgpack: uint32 -> t

type uint64 = int
val uint64_of_msgpack_exn: t -> uint64
val uint64_to_msgpack: uint64 -> t

type float32 = float
val float32_of_msgpack_exn: t -> float32
val float32_to_msgpack: float32 -> t
