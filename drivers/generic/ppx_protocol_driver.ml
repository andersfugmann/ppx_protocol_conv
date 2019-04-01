open Protocol_conv
open Runtime
open StdLabels

module StringMap = Map.Make(String)

module type Parameters = sig
  val field_name: string -> string
  val variant_name: string -> string
  val singleton_constr_as_string: bool
  val omit_default_values: bool
  val eager: bool
  val strict: bool
end

module Default_parameters : Parameters = struct
  let field_name name = name
  let variant_name name = name
  let singleton_constr_as_string = true
  let omit_default_values = true
  let eager = true
  let strict = false
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

  let debug_t = false
  let debug_t t fmt = match debug_t with
    | true -> Printf.eprintf ("%s:" ^^ fmt ^^ "\n%!") (Driver.to_string_hum t)
    | false -> Printf.ifprintf stderr fmt [@@warning "-32"]

  let debug = true
  let debug fmt = match debug with
    | true -> Printf.eprintf (fmt ^^ "\n%!")
    | false -> Printf.ifprintf stderr fmt [@@warning "-32"]

  exception Protocol_error of string * t
  (* Register exception printer *)
  let () = Printexc.register_printer (function
      | Protocol_error (s, t) -> Some (Printf.sprintf "%s, %s" s (Driver.to_string_hum t))
      | _ -> None)

  let to_string_hum = Driver.to_string_hum

  let raise_errorf t fmt =
    Printf.kprintf (fun s -> raise (Protocol_error (s, t))) fmt

  let to_record: type a b. (t, a, b) Record_in.t -> a -> t -> b = fun spec ->
    let rec inner: type a b. (t, a, b) Record_in.t -> orig:t -> a -> 'c -> b =
      let open Record_in in
      function
      | Cons ((field, to_value_func, default), xs) ->
        let field = P.field_name field in
        let cont = inner xs in
        fun ~orig constr t ->
          let v =
            match StringMap.find field t with
            | t ->
              to_value_func t
            | exception Not_found -> begin
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
        Driver.to_alist t
        |> List.fold_left
          ~f:(fun m (k, v) -> StringMap.add k v m)
          ~init:StringMap.empty
      in
      f constr ~orig:t values

  let rec of_record': type a. ((string * t) list -> t) -> (t, a, t) Record_out.t -> (string * t) list -> a = fun f -> function
    | Record_out.Cons ((field, to_t, default), xs) ->
      let field = P.field_name field in
      let cont = of_record' f xs in
      fun acc v -> begin
          match default with
          | Some d when d = v -> cont acc
          | _ -> cont ((field, to_t v) :: acc)
        end
    | Record_out.Nil ->
      fun acc -> f acc

  let of_record spec = of_record' Driver.of_alist spec []

  let rec to_tuple': type a b. (t, a, b) Tuple_in.t -> a -> t list -> b =
    function
    | Tuple_in.Cons (to_value_func, xs) ->
      let cont = to_tuple' xs in
      fun constructor -> begin function
          | t :: ts ->
            let v = to_value_func t in
            cont (constructor v) ts
          | [] -> raise_errorf (Driver.of_list []) "Too few elements when parsing tuple"
        end
    | Tuple_in.Nil -> fun a -> begin
        function [] ->
                  a
               | ts -> raise_errorf (Driver.of_list ts) "Too many elements when parsing tuple"
      end

  let to_tuple spec constructor =
    let to_tuple = to_tuple' spec constructor in
    fun t ->
      to_tuple (Driver.to_list t)

  let rec of_tuple': type a. (t, a, t) Tuple_out.t -> t list -> a = function
    | Tuple_out.Cons (to_t, xs) ->
      let cont = of_tuple' xs in
      fun acc v ->
        cont (to_t v :: acc)
    | Tuple_out.Nil ->
      fun acc -> Driver.of_list (List.rev acc)
  let of_tuple spec = of_tuple' spec []

  let of_variant: type a. string -> (t, a, t) Variant_out.t -> a = fun name ->
    let name = P.variant_name name |> Driver.of_string in
    function
    | Variant_out.Record spec -> of_record' (fun r -> Driver.of_list [ name; Driver.of_alist r ])  spec [ ]
    | Variant_out.Tuple Tuple_out.Nil when P.singleton_constr_as_string -> name
    | Variant_out.Tuple Tuple_out.Nil -> Driver.of_list [ name ]
    | Variant_out.Tuple spec -> of_tuple' spec [ name ]

  let map_variant_constr: type c. (t, c) Variant_in.t -> (t -> c) = function
    | Variant_in.Record (spec, constructor) ->
      begin
        let f = to_record spec constructor in
        fun t -> match Driver.to_list t with
          | [] -> raise_errorf t "Need exactly one argument for parsing record argument of variant"
          | [t] -> f t
          | _ -> raise_errorf t "Too many arguments for parsing record of variant"
      end
    | Variant_in.Tuple (spec, constructor) ->
      let f = to_tuple spec constructor in
      fun t -> f t

  let to_variant: (string * (t, 'c) Variant_in.t) list -> t -> 'c = fun spec ->
    let map = List.fold_left ~init:StringMap.empty ~f:(fun acc (name, spec) ->
        StringMap.add (P.variant_name name) (map_variant_constr spec) acc
      ) spec
    in
    fun t ->
      let name, args = match t with
        | t when P.singleton_constr_as_string && Driver.is_string t -> t, []
        | t when Driver.is_list t -> begin
            match Driver.to_list t with
            | t :: ts when Driver.is_string t -> t, ts
            | _ :: _ -> raise_errorf t "First element in variant list must be a string"
            | [] ->  raise_errorf t "Nonempty list required for variant"
          end
        | _ when P.singleton_constr_as_string -> raise_errorf t "Variant must be a string or list"
        | _ -> raise_errorf t "Variant must be a list"
      in
      let name = Driver.to_string name in
      match StringMap.find name map with
      | f ->
        f (Driver.of_list args)
      | exception _ -> raise_errorf t "Unknown constructor name: %s" name

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
