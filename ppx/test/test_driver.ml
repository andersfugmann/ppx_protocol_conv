open Base
open Protocol_conv
open Runtime
type v =
  | Variant of (string * v list)
  | Record of (string * v) list
  | Tuple of (string * v) list
  | Option of v option
  | List of v list
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | String of string
  | Float of float
  | Char of char
  | Bool of bool
  | Unit

type t = v

exception Protocol_error of string * t

let to_string_hum _ = failwith "Not implemented"

let raise_errorf t fmt =
  Caml.Printf.kprintf (fun s -> raise (Protocol_error (s, t))) fmt

let of_variant destruct t = Variant (destruct t)

let to_variant constr (t : t) =
  match t with
  | Variant v -> constr v
  | e -> raise_errorf e "Variant type not found"

let rec to_record: type a b. (t, a, b) Runtime.Record_in.t -> a -> t -> b =
  let open Runtime.Record_in in
  function
  | Cons ((field_name, to_value_func, default), xs) -> begin
      fun constr -> function
        | Record t as e ->
          let constr =
            let v = match List.Assoc.find t ~equal:String.equal field_name, default with
              | None, Some v -> v
              | Some v, _ ->  to_value_func v
              | None, None -> raise_errorf e "Cannot find field name: %s" field_name
            in
            constr v
          in
          to_record xs constr (Record t)
        | e -> raise_errorf e "Record type not found"
    end
  | Nil -> fun a _t -> a

let of_record: _ Record_out.t -> t = fun l ->
  let rec inner: _ Record_out.t -> (string * t) list = function
    | Record_out.Cons ((_, v, _, Some default), xs) when (Poly.(=) v default) -> inner xs
    | Record_out.Cons ((k, v, to_t, _), xs) -> (k, to_t v) :: inner xs
    | Record_out.Nil -> []
  in
  Record (inner l)

let rec to_tuple: type a b. (t, a, b) Record_in.t -> a -> t -> b =
  function
  | Record_in.Cons ((field, to_value_func, _default), xs) -> begin
      fun constr -> function
        | Tuple ((fn, v) :: ts) when String.equal fn field ->
          to_tuple xs (constr (to_value_func v)) (Tuple ts)
        | Tuple _ as e -> raise_errorf e "Tuple has incorrect ordering"
        | e -> raise_errorf e "Tuple type not found"
    end
  | Record_in.Nil -> fun a _t -> a

let of_tuple t = Tuple t

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

let t_of_test t = t
let t_to_test t = t
