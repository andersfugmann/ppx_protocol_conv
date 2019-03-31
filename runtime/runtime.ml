module Record_in = struct
  type (_, _, _) t =
    | Cons : (string * ('t -> 'a) * 'a option) * ('t, 'b, 'c) t -> ('t, 'a -> 'b, 'c) t
    | Nil : ('t, 'a, 'a) t
  let (^::) a b = Cons (a,b)
end

module Record_out = struct
  type (_, _, _) t =
    | Cons : (string * ('a -> 't) * 'a option) * ('t, 'b, 'c)  t -> ('t, 'a -> 'b, 'c) t
    | Nil : ('t, 'a, 'a) t
  let (^::) a b = Cons (a,b)
end

module Tuple_in = struct
  type (_, _, _) t =
    | Cons : ('t -> 'a) * ('t, 'b, 'c) t -> ('t, 'a -> 'b, 'c) t
    | Nil : ('t, 'a, 'a) t
  let (^::) a b = Cons (a,b)
end

module Tuple_out = struct
  type (_, _, _) t =
    | Cons : ('a -> 't) * ('t, 'b, 'c)  t -> ('t, 'a -> 'b, 'c) t
    | Nil : ('t, 'a, 'a) t
  let (^::) a b = Cons (a,b)
end

(** How does the Nil tuple work?
    Would it be given a t? It throws it away, but it would
    be interesting to see it. Lets build the first version first.
    Should we create aliases. I think its best
*)
module Variant_in = struct
  type  (_, _) t =
    | Tuple:  ('a, 'b, 'c) Tuple_in.t  * 'b -> ('a, 'c) t
    | Record: ('a, 'b, 'c) Record_in.t * 'b -> ('a, 'c) t
end

module Variant_out = struct
  type ('a, 'b, 'c) t =
    | Tuple of ('a, 'b, 'c) Tuple_out.t
    | Record of ('a, 'b, 'c) Record_out.t
end

module type Driver = sig
  type t
  exception Protocol_error of string * t
  val to_string_hum: t -> string

  val to_variant: (string * (t, 'c) Variant_in.t) list -> t -> 'c
  val of_variant: string -> (t, 'a, t) Variant_out.t -> 'a
  val to_record:  (t, 'a, 'b) Record_in.t -> 'a -> t -> 'b
  val of_record:  (t, 'a, t) Record_out.t -> 'a
  val to_tuple:   (t, 'a, 'b) Tuple_in.t -> 'a -> t -> 'b
  val of_tuple:   (t, 'a, t) Tuple_out.t -> 'a

  val to_option:  (t -> 'a) -> t -> 'a option
  val of_option:  ('a -> t) -> 'a option -> t
  val to_ref:     (t -> 'a) -> t -> 'a ref
  val of_ref:     ('a -> t) -> 'a ref -> t
  val to_list:    (t -> 'a) -> t -> 'a list
  val of_list:    ('a -> t) -> 'a list -> t
  val to_array:   (t -> 'a) -> t -> 'a array
  val of_array:   ('a -> t) -> 'a array -> t
  val to_lazy_t:  (t -> 'a) -> t -> 'a lazy_t
  val of_lazy_t:  ('a -> t) -> 'a lazy_t -> t
  val to_int:     t -> int
  val of_int:     int -> t
  val to_int32:   t -> int32
  val of_int32:   int32 -> t
  val to_int64:   t -> int64
  val of_int64:   int64 -> t
  val to_char:    t -> char
  val of_char:    char -> t
  val to_string:  t -> string
  val of_string:  string -> t
  val to_float:   t -> float
  val of_float:   float -> t
  val to_bool:    t -> bool
  val of_bool:    bool -> t
  val to_unit:    t -> unit
  val of_unit:    unit -> t
end
