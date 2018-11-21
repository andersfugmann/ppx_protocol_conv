open Protocol_conv
open StdLabels

module StringMap = Map.Make(String)

module type Driver = sig
  type t
  val to_string_hum: t -> string

  val to_list: t -> t list
  val of_list: t list -> t
  val is_list: t -> bool

  val to_array: t -> t array
  val of_array: t array -> t

  val to_alist: t -> (string * t) list
  val of_alist: (string * t) list -> t
  val is_alist: t -> bool

  val to_int: t -> int
  val of_int: int -> t

  val to_int32: t -> int32
  val of_int32: int32 -> t

  val to_int64: t -> int64
  val of_int64: int64 -> t

  val to_float: t -> float
  val of_float: float -> t

  val to_string: t -> string
  val of_string: string -> t
  val is_string: t -> bool

  val to_bool: t -> bool
  val of_bool: bool -> t

  val null: t
  val is_null: t -> bool
end

let string_map ~f str =
  let cs = ref [] in
  String.iter ~f:(fun c -> cs := c :: !cs) str;
  let cs = f (List.rev !cs) in
  let bytes = Bytes.create (List.length cs) in
  List.iteri ~f:(fun i c -> bytes.[i] <- c) cs;
  Bytes.to_string bytes

(* Convert a_bcd_e_ to aBcdE *)
let mangle: string -> string = fun s ->
  let rec inner : char list -> char list = function
    | '_' :: c :: cs -> (Char.uppercase c) :: (inner cs)
    | '_' :: [] -> []
    | c :: cs -> c :: (inner cs)
    | [] -> []
  in
  string_map ~f:inner s

module Make(Driver: Driver) = struct
  type t = Driver.t
  type flag = [ `Mangle of (string -> string) ]
  type 'a flags = ?flags:flag -> 'a


  exception Protocol_error of string * t
  (* Register exception printer *)
  let () = Printexc.register_printer (function
      | Protocol_error (s, t) -> Some (Printf.sprintf "%s, %s" s (Driver.to_string_hum t))
      | _ -> None)

  let raise_errorf t fmt =
    Printf.kprintf (fun s -> raise (Protocol_error (s, t))) fmt

  let of_variant ?flags:_ destruct t =
    match destruct t with
    | name, [] -> Driver.of_string name
    | name, args -> Driver.of_list (Driver.of_string name :: args)

  let to_variant ?flags:_ constr =
    function
    | t when Driver.is_string t -> constr (Driver.to_string t, [])
    | t when Driver.is_list t -> begin
        match Driver.to_list t with
        | name :: ts when Driver.is_string name ->
          constr (Driver.to_string name, ts)
        | _ -> raise_errorf t "Variant list must start with a string"
      end
    | t -> raise_errorf t "Variants must be a string or a list"

  (* Get all the strings, and create a mapping from string to id? *)
  let to_record: type a b. ?flags:flag -> (t, a, b) Runtime.structure -> a -> t -> b = fun ?flags spec constr ->
    let open Runtime in
    let field_func x = match flags with
      | None -> x
      | Some (`Mangle f) -> f x
    in
    let rec inner: type a b. orig:t -> (t, a, b) Runtime.structure -> a -> 'c -> b = fun ~orig ->
      function
      | Cons ((field, to_value_func), xs) ->
        let field_name = field_func field in
        let cont = inner xs in
        fun constr t ->
          let v = 
            try StringMap.find field_name t |> to_value_func with
            | Not_found -> 
              raise_errorf orig "Field not found: %s" field_name
          in
          cont ~orig (constr v) t
      | Nil -> fun a _t -> a
    in
    let f = inner spec constr in
    fun t ->
      let values =
        Driver.to_alist t
        |> List.fold_left
          ~f:(fun m (k, v) -> StringMap.add k v m)
          ~init:StringMap.empty
      in
      f ~orig:t values

  let of_record: ?flags:flag -> (string * t) list -> t = fun ?flags assoc ->
    let assoc = match flags with
      | None -> assoc
      | Some `Mangle mangle ->
        List.map ~f:(fun (k, v) -> (mangle k, v)) assoc
    in
    Driver.of_alist assoc

  let rec to_tuple: type a b. ?flags:flag -> (t, a, b) Runtime.structure -> a -> t -> b =
    fun ?flags ->
      let open Runtime in
      function
      | Cons ((_field, to_value_func), xs) ->
        fun constructor t ->
          let l = Driver.to_list t in
          let v = to_value_func (List.hd l) in
          to_tuple ?flags xs (constructor v) (Driver.of_list (List.tl l))
      | Nil -> fun a _t -> a

  let of_tuple ?flags:_ t = Driver.of_list (List.map ~f:snd t)

  let get_option = function
    | t when Driver.is_alist t -> begin
        match Driver.to_alist t with
        | [("__option", t)] -> Some t
        | _ -> None
      end
    | _ -> None


  (* If the type is an empty list, thats also null. *)
  let to_option: ?flags:flag -> (t -> 'a) -> t -> 'a option = fun ?flags:_ to_value_fun -> function
    | t when Driver.is_null t -> None
    | t ->
      let t = match (get_option t) with Some t -> t | None -> t in
      Some (to_value_fun t)


  let of_option: ?flags:flag -> ('a -> t) -> 'a option -> t = fun ?flags:_ of_value_fun -> function
    | None -> Driver.null
    | Some v ->
      let mk_option t = Driver.of_alist [ ("__option", t) ] in
      match of_value_fun v with
      | t when Driver.is_null t -> mk_option t
      | t when (get_option t) <> None ->
        mk_option t
      | t -> t

  let to_list: ?flags:flag -> (t -> 'a) -> t -> 'a list = fun ?flags:_ to_value_fun t ->
    List.map ~f:to_value_fun (Driver.to_list t)
  let of_list: ?flags:flag -> ('a -> t) -> 'a list -> t = fun ?flags:_ of_value_fun v ->
    List.map ~f:of_value_fun v |> Driver.of_list

  let to_array: ?flags:flag -> (t -> 'a) -> t -> 'a array = fun ?flags:_ to_value_fun t ->
    to_list to_value_fun t |> Array.of_list

  let of_array: ?flags:flag -> ('a -> t) -> 'a array -> t = fun ?flags:_ of_value_fun v ->
    Array.to_list v |> of_list of_value_fun

  let to_lazy_t: ?flags:flag -> (t -> 'a) -> t -> 'a lazy_t = fun ?flags:_ to_value_fun t ->
    Lazy.from_fun (fun () -> to_value_fun t)

  let of_lazy_t: ?flags:flag -> ('a -> t) -> 'a lazy_t -> t = fun ?flags:_ of_value_fun v ->
    Lazy.force v |> of_value_fun

  let to_int ?flags:_ t = try Driver.to_int t with _ -> raise_errorf t "int expected"
  let of_int ?flags:_ v = Driver.of_int v

  let to_int32 ?flags:_ t = try Driver.to_int32 t with _ -> raise_errorf t "int32 expected"
  let of_int32 ?flags:_ v = Driver.of_int32 v

  let to_int64 ?flags:_ t = try Driver.to_int64 t with _ -> raise_errorf t "int64 expected"
  let of_int64 ?flags:_ v = Driver.of_int64 v

  let to_string ?flags:_ t = try Driver.to_string t with _ -> raise_errorf t "string expected"
  let of_string ?flags:_ v = Driver.of_string v

  let to_float ?flags:_ t = try Driver.to_float t with _ -> raise_errorf t "float expected"
  let of_float ?flags:_ v = Driver.of_float v

  let to_bool ?flags:_ t = try Driver.to_bool t with _ -> raise_errorf t "bool expected"
  let of_bool ?flags:_ v = Driver.of_bool v

  let to_unit ?flags t = to_tuple ?flags Runtime.Nil () t
  let of_unit ?flags () = of_tuple ?flags []
end
