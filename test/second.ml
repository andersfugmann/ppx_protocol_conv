(* type other = {
  field: int;
  xs: int list;
} [@@deriving protocol]

module X = struct
  type b = int [@@deriving protocol]

  module Y = struct
    type c = int [@@deriving protocol]
  end
end
type x = X.b [@@deriving protocol]

type y = X.Y.c [@@deriving protocol]

type z =
  | A of int
  | B of string * int
  | C of z [@@deriving protocol]

type zz = [ `A of int] [@@deriving protocol]
let () = print_endline __FILE__
*)
