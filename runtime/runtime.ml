type (_, _, _) structure =
  | Cons: (string * ('t -> 'a)) * ('t, 'b, 'c) structure -> ('t, 'a -> 'b, 'c) structure
  | Nil : ('t, 'a, 'a) structure

let ( ^:: ) a b = Cons (a, b)

type 'a no_flags = 'a

module type Driver = sig
  type t
  type 'a flags
  exception Protocol_error of string * t

  val to_variant: ((string * t list -> 'a) -> t -> 'a) flags
  val of_variant: (('a -> string * t list) -> 'a -> t) flags
  val to_record:  ((t, 'a, 'b) structure -> 'a -> t -> 'b) flags
  val of_record:  ((string * t) list -> t) flags
  val to_tuple:   ((t, 'a, 'b) structure -> 'a -> t -> 'b) flags
  val of_tuple:   ((string * t) list -> t) flags
  val to_option:  ((t -> 'a) -> t -> 'a option) flags
  val of_option:  (('a -> t) -> 'a option -> t) flags
  val to_list:    ((t -> 'a) -> t -> 'a list) flags
  val of_list:    (('a -> t) -> 'a list -> t) flags
  val to_lazy_t:  ((t -> 'a) -> t -> 'a lazy_t) flags
  val of_lazy_t:  (('a -> t) -> 'a lazy_t -> t) flags
  val to_int:     (t -> int) flags
  val of_int:     (int -> t) flags
  val to_int32:   (t -> int32) flags
  val of_int32:   (int32 -> t) flags
  val to_int64:   (t -> int64) flags
  val of_int64:   (int64 -> t) flags
  val to_string:  (t -> string) flags
  val of_string:  (string -> t) flags
  val to_float:   (t -> float) flags
  val of_float:   (float -> t) flags
  val to_bool:    (t -> bool) flags
  val of_bool:    (bool -> t) flags
  val to_unit:    (t -> unit) flags
  val of_unit:    (unit -> t) flags
end
