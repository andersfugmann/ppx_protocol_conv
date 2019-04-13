open Protocol_conv
open Runtime
open StdLabels

module StringMap = Map.Make(String)

module type Parameters = sig
  val field_name: string -> string
  val variant_name: string -> string
  val constructors_without_arguments_as_string: bool
  val omit_default_values: bool
  val eager: bool
  val strict: bool
end

module Default_parameters : Parameters = struct
  let field_name name = name
  let variant_name name = name
  let constructors_without_arguments_as_string = true
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

  val to_nativeint: t -> nativeint
  val of_nativeint: nativeint -> t

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

  exception Protocol_error of string * t option
  (* Register exception printer *)
  let () = Printexc.register_printer (function
      | Protocol_error (s, Some t) -> Some (Printf.sprintf "%s, %s" s (Driver.to_string_hum t))
      | Protocol_error (s, None) -> Some (Printf.sprintf "%s" s)
      | _ -> None)

  let to_string_hum = Driver.to_string_hum

  let raise_errorf t fmt =
    Printf.kprintf (fun s -> raise (Protocol_error (s, t))) fmt

  let to_record: (t, 'a, 'b) Record_in.t -> 'a -> t -> 'b = fun spec constr ->
    let spec = Helper.map_record_in ~field:P.field_name spec in
    let f = Helper.to_record ~strict:P.strict spec constr in
    fun t -> f (Driver.to_alist t)

  let of_record: type a. (t, a, t) Record_out.t -> a = fun spec ->
    let spec = Helper.map_record_out P.field_name spec in
    Helper.of_record ~omit_default:P.omit_default_values Driver.of_alist spec

  let to_tuple: (t, 'a, 'b) Tuple_in.t -> 'a -> t -> 'b = fun spec constr ->
    let f = Helper.to_tuple spec constr in
    fun t -> f (Driver.to_list t)

  let of_tuple: (t, 'a, t) Tuple_out.t -> 'a = fun spec ->
    Helper.of_tuple Driver.of_list spec

  let of_variant: string -> (t, 'a, t) Tuple_out.t -> 'a =
    let of_variant name =
      let name = P.variant_name name |> Driver.of_string in
      function
      | [] when P.constructors_without_arguments_as_string -> name
      | ts -> Driver.of_list (name :: ts)
    in
    fun name spec -> Helper.of_variant of_variant name spec

  let to_variant: (t, 'a) Variant_in.t list -> t -> 'a = fun spec ->
    let f = Helper.to_variant (Helper.map_constructor_names P.variant_name spec) in
    match P.constructors_without_arguments_as_string with
    | true -> begin
        function
        | t when Driver.is_string t -> f (Driver.to_string t) []
        | t when Driver.is_list t -> begin
            match Driver.to_list t with
            | name :: args when Driver.is_string name -> f (Driver.to_string name) args
            | _ :: _ -> raise_errorf (Some t) "First element in the list must be the constructor name when name when deserialising variant"
            | [] -> raise_errorf (Some t) "Empty list found when deserialising variant"
          end
        | t -> raise_errorf (Some t) "Expected list or string when deserialising variant"
      end
    | false -> begin
        function
        | t when Driver.is_list t -> begin
            match Driver.to_list t with
            | name :: args when Driver.is_string name -> f (Driver.to_string name) args
            | _ :: _ -> raise_errorf (Some t) "First element in the list must be the constructor name when name when deserialising variant"
            | [] -> raise_errorf (Some t) "Empty list found when deserialising variant"
          end
        | t -> raise_errorf (Some t) "Expected list when deserialising variant"
      end

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

  let to_lazy_t: (t -> 'a) -> t -> 'a lazy_t = fun to_value_fun ->
    match P.eager with
    | true -> fun t -> Lazy.from_val (to_value_fun t)
    | false -> fun t -> Lazy.from_fun (fun () -> to_value_fun t)

  let of_lazy_t: ('a -> t) -> 'a lazy_t -> t = fun  of_value_fun v ->
    Lazy.force v |> of_value_fun

  let to_char  t = try Driver.to_char t with _ -> raise_errorf (Some t) "char expected"
  let of_char  v = Driver.of_char v

  let to_int  t = try Driver.to_int t with _ -> raise_errorf (Some t) "int expected"
  let of_int  v = Driver.of_int v

  let to_int32  t = try Driver.to_int32 t with _ -> raise_errorf (Some t) "int32 expected"
  let of_int32  v = Driver.of_int32 v

  let to_int64  t = try Driver.to_int64 t with _ -> raise_errorf (Some t) "int64 expected"
  let of_int64  v = Driver.of_int64 v

  let to_nativeint  t = try Driver.to_nativeint t with _ -> raise_errorf (Some t) "nativeint expected"
  let of_nativeint  v = Driver.of_nativeint v

  let to_string  t = try Driver.to_string t with _ -> raise_errorf (Some t) "string expected"
  let of_string  v = Driver.of_string v

  let to_float  t = try Driver.to_float t with _ -> raise_errorf (Some t) "float expected"
  let of_float  v = Driver.of_float v

  let to_bool  t = try Driver.to_bool t with _ -> raise_errorf (Some t) "bool expected"
  let of_bool  v = Driver.of_bool v

  let to_unit t = to_option (fun _ -> ()) t
                  |> function Some _ -> raise_errorf (Some t) "Unit expected"
                            | None -> ()

  let of_unit () = of_option (fun _ -> failwith "Should call with None") None
end
