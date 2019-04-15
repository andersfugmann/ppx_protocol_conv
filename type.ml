open Protocol_conv_json
module Recursive = struct
  type t = Cons of int * t
         | Nil
  [@@deriving protocol ~driver:(module Json)]

  module Nonrec = struct
    type nonrec t = A of t
    [@@deriving protocol ~driver:(module Json)]
  end

end
