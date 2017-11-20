open Base
open Protocol_conv

type t = Msgpck.t
type flag = [ `Mangle of (string -> string) ]
type 'a flags = ?flags:flag -> 'a


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
  | _ -> failwith "Variant type not found"


(* Get all the strings, and create a mapping from string to id? *)
let to_record: type a b. ?flags:flag -> (t, a, b) Runtime.structure -> a -> t -> b = fun ?flags spec constr ->
  let open Runtime in
  let field_func x = match flags with
    | None -> x
    | Some (`Mangle f) -> f x
  in
  let rec inner: type a b. (t, a, b) Runtime.structure -> a -> 'c -> b =
    function
    | Cons ((field, to_value_func), xs) ->
      let field_name = field_func field in
      let cont = inner xs in
      fun constr t ->
        let v = Map.find_exn t field_name |> to_value_func in
        cont (constr v) t
    | Nil -> fun a _t -> a
  in
  let f = inner spec constr in
  fun t ->
    let values = match t with
      | Msgpck.Map m ->
        List.fold_left
          ~init:(Map.Using_comparator.empty ~comparator:String.comparator)
          ~f:(fun m ->
              function | (Msgpck.String key, data) -> Map.add ~key ~data m
                       | _ -> failwith "protocol error")
          m
      | _ -> failwith "protocol error"
    in
    f values

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
        | _ -> failwith "Not a tuple (list)"
    end
    | Nil -> fun a _t -> a

let of_tuple ?flags:_ t = Msgpck.List (List.map ~f:snd t)

let to_option: ?flags:flag -> (t -> 'a) -> t -> 'a option = fun ?flags:_ to_value_fun -> function
  | Msgpck.Nil -> None
  | x -> Some (to_value_fun x)
let of_option: ?flags:flag -> ('a -> t) -> 'a option -> t = fun ?flags:_ of_value_fun -> function
  | None -> Msgpck.Nil
  | Some x -> of_value_fun x

let to_list: ?flags:flag -> (t -> 'a) -> t -> 'a list =
  fun ?flags:_ to_value_fun ->
    function | Msgpck.List l -> List.map ~f:to_value_fun l
             | _ -> failwith "Not a list"
let of_list: ?flags:flag -> ('a -> t) -> 'a list -> t = fun ?flags:_ of_value_fun v ->
  Msgpck.List (List.map ~f:of_value_fun v)

let to_lazy_t: ?flags:flag -> (t -> 'a) -> t -> 'a lazy_t = fun ?flags:_ to_value_fun t -> Lazy.from_fun (fun () -> to_value_fun t)

let of_lazy_t: ?flags:flag -> ('a -> t) -> 'a lazy_t -> t = fun ?flags:_ of_value_fun v ->
  Lazy.force v |> of_value_fun

let to_int ?flags:_ = function Msgpck.Int i -> i
                             | _ -> failwith "Not an int"
let of_int ?flags:_ i = Msgpck.Int i

let to_int32 ?flags:_ = function Msgpck.Int32 i -> i
                               | _ -> failwith "Not an int32"
let of_int32 ?flags:_ i : t = Msgpck.Int32 i

let to_int64 ?flags:_ = function Msgpck.Int64 i -> i
                               | _ -> failwith "Not an int64"
let of_int64 ?flags:_ i : t = Msgpck.Int64 i

let to_float ?flags:_ = function Msgpck.Float f -> f
                               | Msgpck.Float32 i -> Int32.float_of_bits i
                               | _ -> failwith "Not a float"
let of_float ?flags:_ f = Msgpck.Float f

let to_string ?flags:_ = function Msgpck.String s -> s
                             | _ -> failwith "Not a string"
let of_string ?flags:_ s = Msgpck.String s

let to_bool ?flags:_ = function | Msgpck.Bool b -> b
                                | _ -> failwith "Not a book"
let of_bool ?flags:_ b = Msgpck.Bool b

let to_unit ?flags t = to_tuple ?flags Runtime.Nil () t
let of_unit ?flags () = of_tuple ?flags []

let t_to_msgpack t = t
let t_of_msgpack t = t
