type (_, _, _) structure =
  | Cons: (string * ('t -> 'a)) * ('t, 'b, 'c) structure -> ('t, 'a -> 'b, 'c) structure
  | Nil : ('t, 'a, 'a) structure

let ( ^:: ) a b = Cons (a, b)




module type Driver = sig
  type t
  type flags
  val to_record: flags:flags option -> (t, 'a, 'b) structure -> 'a -> t -> 'b
  val of_record: flags:flags option -> (string * t) list -> t
  val to_tuple:  flags:flags option -> (t, 'a, 'b) structure -> 'a -> t -> 'b
  val of_tuple:  flags:flags option -> (string * t) list -> t
  val to_option: flags:flags option -> (t -> 'a) -> t -> 'a option
  val of_option: flags:flags option -> ('a -> t) -> 'a option -> t
  val to_list:   flags:flags option -> (t -> 'a) -> t -> 'a list
  val of_list:   flags:flags option -> ('a -> t) -> 'a list -> t
  val to_int:    flags:flags option -> t -> int
  val of_int:    flags:flags option -> int -> t
  val to_string: flags:flags option -> t -> string
  val of_string: flags:flags option -> string -> t
  val to_float:  flags:flags option -> t -> float
  val of_float:  flags:flags option -> float -> t
  val to_bool:   flags:flags option -> t -> bool
  val of_bool:   flags:flags option -> bool -> t
  val to_unit:   flags:flags option -> t -> unit
  val of_unit:   flags:flags option -> unit -> t
end
