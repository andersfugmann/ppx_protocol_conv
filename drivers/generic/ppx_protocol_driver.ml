open Protocol_conv
open Runtime
open Base

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

let mangle str =
  match String.split_on_chars ~on:['_'] str with
  | s :: sx ->
    String.concat ~sep:""
      (s :: List.map ~f:String.capitalize sx)
  | _ -> str

module Make(Driver: Driver)(P: Parameters) = struct
  type t = Driver.t
  type error = string * t option
  exception Protocol_error of error

  let make_error ?value msg = (msg, value)

  let error_to_string_hum: error -> string = function
    | (s, Some t) -> Printf.sprintf "%s. Got: %s" s (Driver.to_string_hum t)
    | (s, None) -> s

  (* Register exception printer *)
  let () = Printexc.register_printer (function
      | Protocol_error err -> Some (error_to_string_hum err)
      | _ -> None)

  let to_string_hum = Driver.to_string_hum

  let raise_errorf t fmt =
    Caml.Printf.kprintf (fun s -> raise (Protocol_error (s, t))) fmt

  let try_with: (t -> 'a) -> t -> ('a, error) Runtime.result = fun f t ->
    match f t with
    | v -> Ok v
    | exception (Protocol_error e) -> Error e

  let wrap t f x = match f x with
    | v -> v
    | exception Helper.Protocol_error s -> raise (Protocol_error (s, Some t))

  let to_record: (t, 'a, 'b) Record_in.t -> 'a -> t -> 'b = fun spec constr ->
    let spec = Helper.map_record_in P.field_name spec in
    let f = Helper.to_record ~strict:P.strict spec constr in
    fun t -> wrap t f (Driver.to_alist t)

  let of_record: type a. (t, a, t) Record_out.t -> a = fun spec ->
    let spec = Helper.map_record_out P.field_name spec in
    Helper.of_record ~omit_default:P.omit_default_values Driver.of_alist spec

  let to_tuple: (t, 'a, 'b) Tuple_in.t -> 'a -> t -> 'b = fun spec constr ->
    let f = Helper.to_tuple spec constr in
    fun t -> wrap t f (Driver.to_list t)

  let of_tuple: (t, 'a, t) Tuple_out.t -> 'a = fun spec ->
    Helper.of_tuple Driver.of_list spec

  let to_variant: (t, 'a) Variant_in.t list -> t -> 'a = fun spec ->
    let f = Helper.to_variant (Helper.map_constructor_names P.variant_name spec) in

    match P.constructors_without_arguments_as_string with
    | true -> begin
        function
        | t when Driver.is_string t -> wrap t (f (Driver.to_string t)) []
        | t when Driver.is_list t -> begin
            match Driver.to_list t with
            | name :: args when Driver.is_string name -> wrap t f ((Driver.to_string name)) args
            | _ :: _ -> raise_errorf (Some t) "First element in the list must be the constructor name when name when deserialising variant"
            | [] -> raise_errorf (Some t) "Empty list found when deserialising variant"
          end
        | t -> raise_errorf (Some t) "Expected list or string when deserialising variant"
      end
    | false -> begin
        function
        | t when Driver.is_list t -> begin
            match Driver.to_list t with
            | name :: args when Driver.is_string name -> wrap t (f (Driver.to_string name)) args
            | _ :: _ -> raise_errorf (Some t) "First element in the list must be the constructor name when name when deserialising variant"
            | [] -> raise_errorf (Some t) "Empty list found when deserialising variant"
          end
        | t -> raise_errorf (Some t) "Expected list when deserialising variant"
      end

  let of_variant: string -> (t, 'a, t) Tuple_out.t -> 'a =
    let of_variant name =
      let name = P.variant_name name |> Driver.of_string in
      function
      | [] when P.constructors_without_arguments_as_string -> name
      | ts -> Driver.of_list (name :: ts)
    in
    fun name spec -> Helper.of_variant of_variant name spec

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

  let of_option: ('a -> t) -> 'a option -> t = fun of_value_fun -> function
    | None -> Driver.null
    | Some v ->
      let mk_option t = Driver.of_alist [ ("__option", t) ] in
      match of_value_fun v with
      | t when Driver.is_null t -> mk_option t
      | t when Option.is_some (get_option t) ->
        mk_option t
      | t -> t

  let to_ref: (t -> 'a) -> t -> 'a ref = fun  to_value_fun t ->
      let v = to_value_fun t in
      ref v

  let of_ref: ('a -> t) -> 'a ref -> t = fun  of_value_fun v ->
    of_value_fun !v

  let to_list: (t -> 'a) -> t -> 'a list = fun  to_value_fun t ->
    List.map ~f:to_value_fun (wrap t Driver.to_list t)

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

  let of_lazy_t: ('a -> t) -> 'a lazy_t -> t = fun of_value_fun v ->
    Lazy.force v |> of_value_fun

  let to_char t = try Driver.to_char t with _ -> raise_errorf (Some t) "char expected"
  let of_char = Driver.of_char

  let to_int t = try Driver.to_int t with _ -> raise_errorf (Some t) "int expected"
  let of_int = Driver.of_int

  let to_int32 t = try Driver.to_int32 t with _ -> raise_errorf (Some t) "int32 expected"
  let of_int32 = Driver.of_int32

  let to_int64 t = try Driver.to_int64 t with _ -> raise_errorf (Some t) "int64 expected"
  let of_int64 = Driver.of_int64

  let to_nativeint t = try Driver.to_nativeint t with _ -> raise_errorf (Some t) "nativeint expected"
  let of_nativeint = Driver.of_nativeint

  let to_string t = try Driver.to_string t with _ -> raise_errorf (Some t) "string expected"
  let of_string = Driver.of_string

  let to_float t = try Driver.to_float t with _ -> raise_errorf (Some t) "float expected"
  let of_float = Driver.of_float

  let to_bool t = try Driver.to_bool t with _ -> raise_errorf (Some t) "bool expected"
  let of_bool = Driver.of_bool

  let to_unit t = to_option (fun _ -> ()) t
                  |> function Some _ -> raise_errorf (Some t) "Unit expected"
                            | None -> ()

  let of_unit () = of_option (fun _ -> failwith "Should call with None") None
end
