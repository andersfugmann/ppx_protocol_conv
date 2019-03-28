module Test = struct
  type (_, _) spec =
    | (::) : (string * ('a -> string))  * ('b, 'c) spec -> ('a -> 'b, 'c) spec
    | [] : ('a, 'a) spec

  let rec print: type b. (b, (string * string) list) spec -> (string * string) list -> b = function
    | (name, f) :: xs ->
      let cont = print xs in
      fun acc v -> cont ((name, f v) :: acc)
    | [] -> fun v -> v

  let x = print ["A", string_of_int; "B", string_of_float] []
  let y = (x 4 5.6)
end

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

module type Driver = sig
  type t
  exception Protocol_error of string * t
  val to_string_hum: t -> string
  (* Use gadt *)
  val to_variant: (string * t list -> 'a) -> t -> 'a
  val of_variant: ('a -> string * t list) -> 'a -> t
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
