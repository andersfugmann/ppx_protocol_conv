open Protocol_conv
open Runtime
open StdLabels

module StringMap = Map.Make(String)

module type Parameters = sig
  val field_name: string -> string
  val singleton_constr_as_string: bool
  val omit_default_values: bool
end

module Default_parameters : Parameters = struct
  let field_name name = name
  let singleton_constr_as_string = true
  let omit_default_values = true
end

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

  val to_char:  t -> char
  val of_char:  char -> t

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

module Make(Driver: Driver)(P: Parameters) = struct
  type t = Driver.t

  exception Protocol_error of string * t
  (* Register exception printer *)
  let () = Printexc.register_printer (function
      | Protocol_error (s, t) -> Some (Printf.sprintf "%s, %s" s (Driver.to_string_hum t))
      | _ -> None)

  let to_string_hum = Driver.to_string_hum

  let raise_errorf t fmt =
    Printf.kprintf (fun s -> raise (Protocol_error (s, t))) fmt

  let of_variant destruct t =
    match destruct t with
    | name, [] when P.singleton_constr_as_string ->
      Driver.of_string name
    | name, args -> Driver.of_string name :: args |> Driver.of_list

  let to_variant constr =
    function
    | t when P.singleton_constr_as_string && Driver.is_string t -> constr (Driver.to_string t, [])
    | t when Driver.is_list t -> begin
        match Driver.to_list t with
        | name :: ts when Driver.is_string name ->
          constr (Driver.to_string name, ts)
        | _ -> raise_errorf t "Variant list must start with a string"
      end
    | t when P.singleton_constr_as_string -> raise_errorf t "Variants must be a string or a list"
    | t -> raise_errorf t "Variants must be a list"

  let to_record: type a b. (t, a, b) Record_in.t -> a -> t -> b = fun spec constr ->
    let rec inner: type a b. orig:t -> (t, a, b) Record_in.t -> a -> 'c -> b = fun ~orig ->
      let open Record_in in
      function
      | Cons ((field, to_value_func, default), xs) ->
        let field_name = P.field_name field in
        let cont = inner xs in
        fun constr t ->
          let v =
            try StringMap.find field_name t |> to_value_func with
            | Not_found -> begin
                match default with
                | None -> raise_errorf orig "Field not found: %s" field_name
                | Some v -> v
              end
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

  let of_record: _ Record_out.t -> t = fun l ->
    let rec inner: _ Record_out.t -> (string * t) list = function
      | Record_out.Cons ((_, v, _, Some default), xs) when P.omit_default_values && v = default -> inner xs
      | Record_out.Cons ((k, v, to_t, _), xs) -> (P.field_name k, to_t v) :: inner xs
      | Record_out.Nil -> []
    in
    let assoc = inner l in
    Driver.of_alist assoc

  let rec to_tuple: type a b. (t, a, b) Record_in.t -> a -> t -> b =
      let open Record_in in
      function
      | Record_in.Cons ((_field, to_value_func, _default), xs) ->
        fun constructor t ->
          let l = Driver.to_list t in
          let v = to_value_func (List.hd l) in
          to_tuple xs (constructor v) (Driver.of_list (List.tl l))
      | Nil -> fun a _t -> a

  let of_tuple  t = Driver.of_list (List.map ~f:snd t)

  let get_option = function
    | t when Driver.is_alist t -> begin
        match Driver.to_alist t with
        | [("__option", t)] -> Some t
        | _ -> None
      end
    | _ -> None


  (* If the type is an empty list, thats also null. *)
  let to_option: (t -> 'a) -> t -> 'a option = fun  to_value_fun -> function
    | t when Driver.is_null t -> None
    | t ->
      let t = match (get_option t) with Some t -> t | None -> t in
      Some (to_value_fun t)


  let of_option: ('a -> t) -> 'a option -> t = fun  of_value_fun -> function
    | None -> Driver.null
    | Some v ->
      let mk_option t = Driver.of_alist [ ("__option", t) ] in
      match of_value_fun v with
      | t when Driver.is_null t -> mk_option t
      | t when (get_option t) <> None ->
        mk_option t
      | t -> t

  let to_ref: (t -> 'a) -> t -> 'a ref = fun  to_value_fun t ->
      let v = to_value_fun t in
      ref v

  let of_ref: ('a -> t) -> 'a ref -> t = fun  of_value_fun v ->
    of_value_fun !v

  let to_list: (t -> 'a) -> t -> 'a list = fun  to_value_fun t ->
    List.map ~f:to_value_fun (Driver.to_list t)
  let of_list: ('a -> t) -> 'a list -> t = fun  of_value_fun v ->
    List.map ~f:of_value_fun v |> Driver.of_list

  let to_array: (t -> 'a) -> t -> 'a array = fun  to_value_fun t ->
    to_list to_value_fun t |> Array.of_list

  let of_array: ('a -> t) -> 'a array -> t = fun  of_value_fun v ->
    Array.to_list v |> of_list of_value_fun

  let to_lazy_t: (t -> 'a) -> t -> 'a lazy_t = fun  to_value_fun t ->
    Lazy.from_fun (fun () -> to_value_fun t)

  let of_lazy_t: ('a -> t) -> 'a lazy_t -> t = fun  of_value_fun v ->
    Lazy.force v |> of_value_fun

  let to_char  t = try Driver.to_char t with _ -> raise_errorf t "char expected"
  let of_char  v = Driver.of_char v

  let to_int  t = try Driver.to_int t with _ -> raise_errorf t "int expected"
  let of_int  v = Driver.of_int v

  let to_int32  t = try Driver.to_int32 t with _ -> raise_errorf t "int32 expected"
  let of_int32  v = Driver.of_int32 v

  let to_int64  t = try Driver.to_int64 t with _ -> raise_errorf t "int64 expected"
  let of_int64  v = Driver.of_int64 v

  let to_string  t = try Driver.to_string t with _ -> raise_errorf t "string expected"
  let of_string  v = Driver.of_string v

  let to_float  t = try Driver.to_float t with _ -> raise_errorf t "float expected"
  let of_float  v = Driver.of_float v

  let to_bool  t = try Driver.to_bool t with _ -> raise_errorf t "bool expected"
  let of_bool  v = Driver.of_bool v

  let to_unit t = to_option (fun _ -> ()) t
                  |> function Some _ -> raise_errorf t "Unit (null) expected"
                            | None -> ()

  let of_unit () = of_option (fun _ -> failwith "Should call with None") None
end
