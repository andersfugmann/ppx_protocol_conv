open Base
open Protocol_conv
open Protocol_conv.Runtime
type t = Global.Yaml.value
type 'a flags = 'a no_flags

exception Protocol_error of string * t

let string_of_t value =
  match Global.Yaml.to_string value with
  | Ok s -> s
  | Error (`Msg msg) -> failwith msg


(* Register exception printer *)
let () = Caml.Printexc.register_printer
    (function Protocol_error (s, t) -> Some (s ^ ", " ^ (string_of_t t))
            | _ -> None)

let raise_errorf t fmt =
  Caml.Printf.kprintf (fun s -> raise (Protocol_error (s, t))) fmt

module Util = struct
  let to_list = function
    | `A l -> l
    | t -> raise_errorf t "Excpected list"

  let to_assoc = function
    | `O l -> l
    | t -> raise_errorf t "Excpected assoc"

  let to_float = function
    | `Float f -> f
    | t -> raise_errorf t "Float expected"
  let to_int = function
    | `Float f -> Base.Float.to_int f
    | t -> raise_errorf t "Int expected"
  let to_string = function
    | `String s -> s
    | t -> raise_errorf t "String expected"
  let to_bool = function
    | `Bool b -> b
    | t -> raise_errorf t "Bool expected"

end

let of_variant destruct t =
  match destruct t with
  | name, [] -> `String name
  | name, args -> `A (`String name :: args)

let to_variant constr (t : t) =
  match t with
  | `String name -> constr (name, [])
  | `A (`String name :: ts) -> constr (name, ts)
  | e -> raise_errorf e "Variant type not found"

(* Get all the strings, and create a mapping from string to id? *)
let to_record: type a b. (t, a, b) Runtime.structure -> a -> t -> b = fun spec constr ->
  let open Runtime in
  let rec inner: type a b. (t, a, b) Runtime.structure -> a -> 'c -> b =
    function
    | Cons ((field_name, to_value_func), xs) ->
      let cont = inner xs in
      fun constr t ->
        let v = Map.find_exn t field_name |> to_value_func in
        cont (constr v) t
    | Nil -> fun a _t -> a
  in
  let f = inner spec constr in
  fun t ->
    let values =
      Util.to_assoc t
      |> Map.Using_comparator.of_alist_exn ~comparator:String.comparator
        (*List.fold_left
        ~init:(Map.Using_comparator.empty ~comparator:String.comparator)
          ~f:(fun m (key, data) -> Map.add_exn ~key ~data m)*)
    in
    f values

let of_record: (string * t) list -> t = fun assoc -> `O assoc

let rec to_tuple: type a b. (t, a, b) Runtime.structure -> a -> t -> b =
  let open Runtime in
  function
  | Cons ((_field_name, to_value_func), xs) ->
    fun constructor t ->
      let l = Util.to_list t in
      let v = to_value_func (List.hd_exn l) in
      to_tuple xs (constructor v) (`A (List.tl_exn l))
  | Nil -> fun a _t -> a

let of_tuple t = `A (List.map ~f:snd t)

let to_option: (t -> 'a) -> t -> 'a option = fun to_value_fun -> function
  | `Null -> None
  | x -> Some (to_value_fun x)
let of_option: ('a -> t) -> 'a option -> t = fun of_value_fun -> function
  | None -> `Null
  | Some x -> of_value_fun x

let to_list: (t -> 'a) -> t -> 'a list = fun to_value_fun t ->
  List.map ~f:to_value_fun (Util.to_list t)
let of_list: ('a -> t) -> 'a list -> t = fun of_value_fun v ->
  `A (List.map ~f:of_value_fun v)

let to_lazy_t: (t -> 'a) -> t -> 'a lazy_t = fun to_value_fun t -> Lazy.from_fun (fun () -> to_value_fun t)

let of_lazy_t: ('a -> t) -> 'a lazy_t -> t = fun of_value_fun v ->
  Lazy.force v |> of_value_fun

let to_int t = Util.to_int t
let of_int i = `Float (Float.of_int i)

let to_int32 t = Util.to_float t |> Int32.of_float
let of_int32 i = `Float (Int32.to_float i)

let to_int64 t = Util.to_float t |> Int64.of_float
let of_int64 i = `Float (Int64.to_float i)

let to_string t = Util.to_string t
let of_string s = `String s

let to_float t = Util.to_float t
let of_float s = `Float s

let to_bool t = Util.to_bool t
let of_bool b = `Bool b

let to_unit t = to_tuple Runtime.Nil () t
let of_unit () = of_tuple []

(* Allow referencing Yaml.value in structures. *)
let t_of_yaml t = t
let t_to_yaml t = t
