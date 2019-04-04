open Base
open Protocol_conv
open Runtime
type t =
  | Map of (string *t) list
  | Tuple of t list
  | Option of t option
  | List of t list
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | String of string
  | Float of float
  | Char of char
  | Bool of bool
  | Unit
[@@deriving sexp]

module StringMap = Caml.Map.Make(String)

exception Protocol_error of string * t option

let to_string_hum t = sexp_of_t t |> Sexp.to_string_hum

let raise_errorf t fmt =
  Caml.Printf.kprintf (fun s -> raise (Protocol_error (s, Some t))) fmt

let of_list l = List l
let to_string = function String s -> s | t -> raise_errorf t "String expected"
let of_string s = String s
let of_map l = Map l

let to_variant: type c. (string * (t, c) Variant_in.t) list -> t -> c = fun spec ->
  let f = Helper.to_variant (function Map m -> Some m | _ -> None) spec in
  function List (name :: args) -> f (to_string name) args
         | t -> raise_errorf t "List expected"

let of_variant: string -> (t, 'a, t) Variant_out.t -> 'a = fun name ->
  Helper.of_variant (function Helper.Nil -> List [of_string name]
                            | Helper.Tuple args -> List (of_string name :: args)
                            | Helper.Record record -> List [of_string name; Map record]
    )
let to_record: (t, 'constr, 'b) Record_in.t -> 'constr -> t -> 'b = fun spec constr ->
  let f = Helper.to_record spec constr in
  function Map m -> f m
         | t -> raise_errorf t "Expected map for record"

let of_record: type a. (t, a, t) Record_out.t -> a = fun spec ->
  Helper.of_record of_map spec

let to_tuple: (t, 'constr, 'b) Tuple_in.t -> 'constr -> t -> 'b = fun spec constr->
  let f = Helper.to_tuple spec constr in
  function List ts -> f ts
         | t -> raise_errorf t "List expected to tuple"

let of_tuple: type a. (t, a, t) Tuple_out.t -> a = fun spec ->
  Helper.of_tuple of_list spec

let to_option: (t -> 'a) -> t -> 'a option = fun to_value_fun -> function
  | Option None -> None
  | Option (Some v) -> Some (to_value_fun v)
  | e -> raise_errorf e "Option type not found"

let of_option: ('a -> t) -> 'a option -> t = fun of_value_fun -> function
  | None -> Option None
  | Some v -> Option (Some (of_value_fun v))

let to_ref: (t -> 'a) -> t -> 'a ref = fun to_value_fun t ->
  ref (to_value_fun t)

let of_ref: ('a -> t) -> 'a ref -> t = fun of_value_fun v ->
  of_value_fun (!v)

let to_list: (t -> 'a) -> t -> 'a list = fun to_value_fun -> function
  | List vs -> List.map ~f:to_value_fun vs
  | e -> raise_errorf e "List type not found"

let of_list: ('a -> t) -> 'a list -> t = fun of_value_fun v ->
  List (List.map ~f:of_value_fun v)

let to_array: (t -> 'a) -> t -> 'a array = fun to_value_fun t ->
  to_list to_value_fun t |> Array.of_list

let of_array: ('a -> t) -> 'a array -> t = fun of_value_fun v ->
  of_list of_value_fun (Array.to_list v)

let to_lazy_t: (t -> 'a) -> t -> 'a lazy_t = fun to_value_fun t ->
  Lazy.from_fun (fun () -> to_value_fun t)

let of_lazy_t: ('a -> t) -> 'a lazy_t -> t = fun of_value_fun v ->
  Lazy.force v |> of_value_fun

let to_int = function Int i -> i | e -> raise_errorf e "Int type not found"
let of_int i = Int i

let to_int32 = function Int32 i -> i | e -> raise_errorf e "Int32 type not found"
let of_int32 i = Int32 i

let to_int64 = function Int64 i -> i | e -> raise_errorf e "Int64 type not found"
let of_int64 i = Int64 i

let to_string = function String s -> s | e -> raise_errorf e "String type not found"
let of_string s = String s

let to_float = function Float f -> f | e -> raise_errorf e "Float type not found"
let of_float f = Float f

let to_char = function Char c -> c | e -> raise_errorf e "String type not found"
let of_char c = Char c

let to_bool = function Bool b -> b | e -> raise_errorf e "Bool type not found"
let of_bool b = Bool b

let to_unit = function Unit -> () | e -> raise_errorf e "Unit type not found"
let of_unit () = Unit

let to_nativeint = function Nativeint n -> n | e -> raise_errorf e "Nativeint not found"
let of_nativeint n = Nativeint n

let t_of_test t = t
let t_to_test t = t
