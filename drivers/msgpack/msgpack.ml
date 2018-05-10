open Base
open Protocol_conv

type t = Msgpck.t
type flag = [ `Mangle of (string -> string) ]
type 'a flags = ?flags:flag -> 'a

exception Protocol_error of string * t
(* Register exception printer *)
let () = Caml.Printexc.register_printer
    (function Protocol_error (s, t) -> Some (s ^ ", " ^ (Msgpck.show t))
            | _ -> None)

let raise_errorf t fmt =
  Caml.Printf.kprintf (fun s -> raise (Protocol_error (s, t))) fmt

(* Convert a_bcd_e_ to aBcdE *)
let mangle (s : string) =
  let rec inner : char list -> char list = function
    | '_' :: c :: cs -> (Char.uppercase c) :: (inner cs)
    | '_' :: [] -> []
    | c :: cs -> c :: (inner cs)
    | [] -> []
  in
  String.to_list s |> inner |> String.of_char_list

let of_variant ?flags:_ destruct t =
  match destruct t with
  | name, [] -> Msgpck.String name
  | name, args -> Msgpck.List (Msgpck.String name :: args)

let to_variant ?flags:_ constr (t : t) =
  match t with
  | Msgpck.String name -> constr (name, [])
  | Msgpck.List (Msgpck.String name :: ts) -> constr (name, ts)
  | e -> raise_errorf e "Variant type not found"


(* Get all the strings, and create a mapping from string to id? *)
let to_record: type a b. ?flags:flag -> (t, a, b) Runtime.structure -> a -> t -> b = fun ?flags spec constr ->
  let open Runtime in
  let field_func x = match flags with
    | None -> x
    | Some (`Mangle f) -> f x
  in
  let rec inner: type a b. (t, a, b) Runtime.structure -> a -> 'c -> t -> b =
    function
    | Cons ((field, to_value_func), xs) ->
      let field_name = field_func field in
      let cont = inner xs in
      fun constr m t ->
        let v = match Map.find m field_name with
          | Some v -> to_value_func v
          | None -> raise_errorf t "Could not key: %s" field_name
        in
        cont (constr v) m t
    | Nil -> fun a _m _t -> a
  in
  let f = inner spec constr in
  fun t ->
    let values = match t with
      | Msgpck.Map m ->
        m
        |> List.map ~f:(function | (Msgpck.String key, data) -> (key, data)
                                 | (e, _) -> raise_errorf e "string expected")
        |> Map.Using_comparator.of_alist_exn ~comparator:String.comparator
      | e -> raise_errorf e "map expected"
    in
    f values t

let of_record: ?flags:flag -> (string * t) list -> t = fun ?flags assoc ->
  let assoc = match flags with
    | None -> List.map ~f:(fun (k, v) -> (Msgpck.String k, v)) assoc

    | Some `Mangle mangle ->
        List.map ~f:(fun (k, v) -> (Msgpck.String (mangle k), v)) assoc
  in
  Msgpck.Map assoc

let rec to_tuple: type a b. ?flags:flag -> (t, a, b) Runtime.structure -> a -> t -> b =
  let open Runtime in
  fun ?flags -> function
    | Cons ((_field, to_value_func), xs) -> begin
      fun constructor -> function
        | Msgpck.List l ->
          let v = to_value_func (List.hd_exn l) in
          to_tuple ?flags xs (constructor v) (Msgpck.List (List.tl_exn l))
        | e -> raise_errorf e "tuple (list) expected"
    end
    | Nil -> fun a _t -> a

let of_tuple ?flags:_ t = Msgpck.List (List.map ~f:snd t)

let to_option: ?flags:flag -> (t -> 'a) -> t -> 'a option = fun ?flags:_ to_value_fun -> function
  | Msgpck.Nil -> None
  | Msgpck.Map [(Msgpck.String "__option", t)] -> Some (to_value_fun t)
  | x -> Some (to_value_fun x)
let of_option: ?flags:flag -> ('a -> t) -> 'a option -> t = fun ?flags:_ of_value_fun -> function
  | None -> Msgpck.Nil
  | Some x -> begin
      match of_value_fun x with
      | (Msgpck.Nil as t)
      | (Msgpck.Map [Msgpck.String "__option", _ ] as t) ->
        Msgpck.Map [Msgpck.String "__option", t ]
      | x -> x
    end

let to_list: ?flags:flag -> (t -> 'a) -> t -> 'a list =
  fun ?flags:_ to_value_fun ->
    function | Msgpck.List l -> List.map ~f:to_value_fun l
             | e -> raise_errorf e "list expected"
let of_list: ?flags:flag -> ('a -> t) -> 'a list -> t = fun ?flags:_ of_value_fun v ->
  Msgpck.List (List.map ~f:of_value_fun v)

let to_lazy_t: ?flags:flag -> (t -> 'a) -> t -> 'a lazy_t = fun ?flags:_ to_value_fun t -> Lazy.from_fun (fun () -> to_value_fun t)

let of_lazy_t: ?flags:flag -> ('a -> t) -> 'a lazy_t -> t = fun ?flags:_ of_value_fun v ->
  Lazy.force v |> of_value_fun

let to_int ?flags:_ = function Msgpck.Int i -> i
                             | Msgpck.Int32 i -> Int32.to_int_exn i
                             | Msgpck.Uint32 i -> Int32.to_int_exn i
                             | Msgpck.Int64 i -> Int64.to_int_exn i
                             | Msgpck.Uint64 i -> Int64.to_int_exn i
                             | e -> raise_errorf e "int expected"

let of_int ?flags:_ i = Msgpck.Int i

let to_int32 ?flags:_ = function Msgpck.Int32 i -> i
                               | e -> raise_errorf e "int expected32"
let of_int32 ?flags:_ i : t = Msgpck.Int32 i

let to_int64 ?flags:_ = function Msgpck.Int64 i -> i
                               | e -> raise_errorf e "int expected64"
let of_int64 ?flags:_ i : t = Msgpck.Int64 i

let to_float ?flags:_ = function Msgpck.Float f -> f
                               | Msgpck.Float32 i -> Int32.float_of_bits i
                               | e -> raise_errorf e "float expected"
let of_float ?flags:_ f = Msgpck.Float f

let to_string ?flags:_ = function Msgpck.String s -> s
                                | Msgpck.Bytes s -> s
                                | e -> raise_errorf e "string or bytes expected"
let of_string ?flags:_ s = Msgpck.String s

let to_bool ?flags:_ = function Msgpck.Bool b -> b
                              | e -> raise_errorf e "bool expected"
let of_bool ?flags:_ b = Msgpck.Bool b

let to_unit ?flags t = to_tuple ?flags Runtime.Nil () t
let of_unit ?flags () = of_tuple ?flags []

type bytes = string
let bytes_of_msgpack s = to_string s
let bytes_to_msgpack = Msgpck.of_bytes

type uint32 = int
let uint32_of_msgpack = function Msgpck.Uint32 v -> Int32.to_int_exn v
                               | e -> raise_errorf e "uint32 expected"
let uint32_to_msgpack v = Int32.of_int_exn v |> Msgpck.of_uint32

type uint64 = int
let uint64_of_msgpack = function Msgpck.Uint64 v -> Int64.to_int_exn v
                               | e -> raise_errorf e "uint32 expected"
let uint64_to_msgpack v = Int64.of_int_exn v |> Msgpck.of_uint64

type float32 = float
let float32_of_msgpack = function Msgpck.Float32 v -> Int32.float_of_bits v
                               | e -> raise_errorf e "float32 expected"
let float32_to_msgpack v = Int32.bits_of_float v |> Msgpck.of_float32

let to_msgpack t = t
let of_msgpack t = t
