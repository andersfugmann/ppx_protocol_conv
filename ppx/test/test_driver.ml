open Base
open Protocol_conv
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
  | Bool of bool
  | Unit

type t = v
type 'a flags = 'a

exception Protocol_error of string * t

let raise_errorf t fmt =
  Caml.Printf.kprintf (fun s -> raise (Protocol_error (s, t))) fmt

let of_variant destruct t = Variant (destruct t)

let to_variant constr (t : t) =
  match t with
  | Variant v -> constr v
  | e -> raise_errorf e "Variant type not found"


(* Get all the strings, and create a mapping from string to id? *)
let rec to_record: type a b. (t, a, b) Runtime.structure -> a -> t -> b =
  let open Runtime in
  function
  | Cons ((field_name, to_value_func), xs) -> begin
      fun constr -> function
        | Record t ->
          let constr =
            List.Assoc.find_exn t ~equal:String.equal field_name
            |> to_value_func
            |> constr
          in
          to_record xs constr (Record t)
        | e -> raise_errorf e "Record type not found"
    end
  | Nil -> fun a _t -> a


let of_record: (string * t) list -> t = fun assoc -> Record assoc

let rec to_tuple: type a b. (t, a, b) Runtime.structure -> a -> t -> b =
  let open Runtime in
  function
  | Cons ((field, to_value_func), xs) -> begin
      fun constr -> function
        | Tuple ((fn, v) :: ts) when String.equal fn field ->
          to_tuple xs (constr (to_value_func v)) (Tuple ts)
        | Tuple _ as e -> raise_errorf e "Tuple has incorrect ordering"
        | e -> raise_errorf e "Tuple type not found"
    end
  | Nil -> fun a _t -> a

let of_tuple t = Tuple t

let to_option: (t -> 'a) -> t -> 'a option = fun to_value_fun -> function
  | Option None -> None
  | Option (Some v) -> Some (to_value_fun v)
  | e -> raise_errorf e "Option type not found"

let of_option: ('a -> t) -> 'a option -> t = fun of_value_fun -> function
  | None -> Option None
  | Some v -> Option (Some (of_value_fun v))

let to_list: (t -> 'a) -> t -> 'a list = fun to_value_fun -> function
  | List vs -> List.map ~f:to_value_fun vs
  | e -> raise_errorf e "List type not found"

let of_list: ('a -> t) -> 'a list -> t = fun of_value_fun v ->
  List (List.map ~f:of_value_fun v)

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

let to_bool = function Bool b -> b | e -> raise_errorf e "Bool type not found"
let of_bool b = Bool b

let to_unit = function Unit -> () | e -> raise_errorf e "Unit type not found"
let of_unit () = Unit

let t_of_test t = t
let t_to_test t = t
