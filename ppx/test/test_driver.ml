open Protocol_conv
open Runtime
open Base

type t =
  | Record of (string * t) list
  | Variant of string * t list
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

type error = string * t option
exception Protocol_error of error

let make_error ?value msg = (msg, value)

let to_string_hum t = sexp_of_t t |> Sexp.to_string_hum
let error_to_string_hum: error -> string = function
  | (s, Some t) -> Printf.sprintf "%s. T: '%s'" s (to_string_hum t)
  | (s, None) -> s

(* Register exception printer *)
let () = Caml.Printexc.register_printer (function
    | Protocol_error err -> Some (error_to_string_hum err)
    | _ -> None)

let raise_errorf t fmt =
  Caml.Printf.kprintf (fun s -> raise (Protocol_error (s, Some t))) fmt

let try_with: (t -> 'a) -> t -> ('a, error) Runtime.result = fun f t ->
  match f t with
  | v -> Ok v
  | exception (Protocol_error e) -> Error e

let to_variant: (t, 'a) Variant_in.t list -> t -> 'a = fun spec -> function
  | Variant (name, args) -> Helper.to_variant spec name args
  | t -> raise_errorf t "Variant expected"

let of_variant: string -> (t, 'a, t) Tuple_out.t -> 'a = fun name spec ->
  Helper.of_variant (fun name args -> Variant (name, args)) name spec

let to_record: (t, 'constr, 'b) Record_in.t -> 'constr -> t -> 'b = fun spec constr -> function
  | Record rs -> Helper.to_record spec constr rs
  | t -> raise_errorf t "Expected map for record"

let of_record: type a. (t, a, t) Record_out.t -> a = fun spec ->
  Helper.of_record ~omit_default:false (fun rs -> Record rs) spec

let to_tuple: (t, 'constr, 'b) Tuple_in.t -> 'constr -> t -> 'b = fun spec constr->
  let f = Helper.to_tuple spec constr in
  function List ts -> f ts
         | t -> raise_errorf t "List expected to tuple"

let of_tuple: type a. (t, a, t) Tuple_out.t -> a = fun spec ->
  Helper.of_tuple (fun l -> List l) spec

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
