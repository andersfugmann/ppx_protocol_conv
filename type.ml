module X = struct
  type v = [ `A | `B of int | `C of int * int | `D of (int * int) ]
  and t = v list
  [@@deriving protocol ~driver:(module Driver)]
end
