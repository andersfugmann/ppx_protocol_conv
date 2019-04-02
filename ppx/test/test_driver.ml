open Base
open Protocol_conv
open Runtime
type v =
  | Map of (string * v) list
  | Tuple of v list
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

module StringMap = Caml.Map.Make(String)

exception Protocol_error of string * t option

let to_string_hum _ = failwith "Not implemented - Use sexp"

let raise_errorf t fmt =
  Caml.Printf.kprintf (fun s -> raise (Protocol_error (s, Some t))) fmt

let of_list l = List l
let to_list = function
  | List l -> l
  | t -> raise_errorf t "List expected"
let is_list = function List _ -> true | _ -> false

let is_string = function String _ -> true | _ -> false
let to_string = function String s -> s | t -> raise_errorf t "String expected"
let of_string s = String s
let of_map l = Map l
let to_map = function
  | Map l -> l
  | t -> raise_errorf t "List expected"

let to_record: type a b. (t, a, b) Record_in.t -> a -> t -> b = fun spec ->
  let rec inner: type a b. (t, a, b) Record_in.t -> orig:t -> a -> 'c -> b =
    let open Record_in in
    function
    | Cons ((field, to_value_func, default), xs) ->
      let cont = inner xs in
      fun ~orig constr t ->
        let v =
          match StringMap.find field t with
          | t ->
            to_value_func t
          | exception Caml.Not_found -> begin
                match default with
                  | None -> raise_errorf orig "Field not found: %s" field
                | Some v -> v
              end
        in
        cont ~orig (constr v) t
    | Nil -> fun ~orig:_ a _t -> a
  in
  let f = inner spec in
    fun constr t ->
      let values =
        to_map t
        |> List.fold_left
          ~f:(fun m (k, v) -> StringMap.add k v m)
          ~init:StringMap.empty
      in
      f constr ~orig:t values

let rec of_record': type a. ((string * t) list -> t) -> (t, a, t) Record_out.t -> (string * t) list -> a = fun f -> function
  | Record_out.Cons ((field, to_t, default), xs) ->
    let cont = of_record' f xs in
    fun acc v -> begin
        match default with
        | Some d when Poly.equal d  v -> cont acc
        | _ -> cont ((field, to_t v) :: acc)
      end
  | Record_out.Nil ->
    fun acc -> f acc

let of_record spec = of_record' of_map spec []

let rec to_tuple: type a b. (t, a, b) Tuple_in.t -> a -> t -> b =
  function
  | Tuple_in.Cons (to_value_func, xs) ->
    let cont = to_tuple xs in
    fun constructor t ->
      let l = to_list t in
      let v = to_value_func (List.hd_exn l) in
      cont (constructor v) (of_list (List.tl_exn l))
  | Tuple_in.Nil -> fun a _t -> a

let rec of_tuple': type a. (t, a, t) Tuple_out.t -> t list -> a = function
  | Tuple_out.Cons (to_t, xs) ->
    let cont = of_tuple' xs in
    fun acc v ->
      cont (to_t v :: acc)
  | Tuple_out.Nil ->
    fun acc -> of_list (List.rev acc)
let of_tuple spec = of_tuple' spec []

let of_variant: type a. string -> (t, a, t) Variant_out.t -> a = fun name ->
  let name = name |> of_string in
  function
  | Variant_out.Record spec -> of_record' (fun r -> of_list [ name; of_map r ])  spec [ ]
  | Variant_out.Tuple Tuple_out.Nil -> name
  | Variant_out.Tuple spec -> of_tuple' spec [ name ]

let map_variant_constr: type c. (t, c) Variant_in.t -> (t -> c) = function
  | Variant_in.Record (spec, constructor) ->
    begin
      let f = to_record spec constructor in
      fun t -> match to_list t with
        | [] -> raise_errorf t "Need exactly one argument for parsing record argument of variant"
        | [t] -> f t
        | _ -> raise_errorf t "Too many arguments for parsing record of variant"
    end
  | Variant_in.Tuple (spec, constructor) ->
    let f = to_tuple spec constructor in
    fun t -> f t

let to_variant: (string * (t, 'c) Variant_in.t) list -> t -> 'c = fun spec ->
  let map = List.fold_left ~init:StringMap.empty ~f:(fun acc (name, spec) ->
      StringMap.add name (map_variant_constr spec) acc
    ) spec
  in
  fun t ->
    let name, args = match t with
      | t when is_string t -> t, []
      | t when is_list t -> begin
          match to_list t with
          | t :: ts when is_string t -> t, ts
          | _ :: _ -> raise_errorf t "First element in variant list must be a string"
          | [] ->  raise_errorf t "Nonempty list required for variant"
        end
      | _ -> raise_errorf t "Variant must be a string or list"
    in
    let name = to_string name in
    match StringMap.find name map with
    | f -> f (of_list args)
    | exception _ -> raise_errorf t "Unknown constructor name: %s" name


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
