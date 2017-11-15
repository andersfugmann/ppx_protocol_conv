(* Json Protocol *)
exception Dubplicate_key of string * string
val mangle: string -> string

include Deriving_protocol.Runtime.Driver with type t = Yojson.Safe.json and type flags = [ `Mangle of (string -> string) ]
