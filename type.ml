open Protocol_conv_json
module Recursive : sig
  type t = Cons of int * t
         | Nil
  [@@deriving protocol ~driver:(module Json)]
end = struct
  type t = Cons of int * t
         | Nil
  [@@deriving protocol ~driver:(module Json)]
end
