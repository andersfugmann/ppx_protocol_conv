open Base

type ('v, 'e) result = ('v, 'e) Result.t

module Record_in = struct
  type (_, _, _) t =
    | Cons : (string * ('t -> 'a) * 'a option) * ('t, 'b, 'c) t -> ('t, 'a -> 'b, 'c) t
    | Nil : ('t, 'a, 'a) t
  let (^::) a b = Cons (a,b)
end

module Record_out = struct
  type (_, _, _) t =
    | Cons : (string * ('a -> 't) * 'a option) * ('t, 'b, 'c)  t -> ('t, 'a -> 'b, 'c) t
    | Nil : ('t, 'a, 'a) t
  let (^::) a b = Cons (a,b)
end

module Tuple_in = struct
  type (_, _, _) t =
    | Cons : ('t -> 'a) * ('t, 'b, 'c) t -> ('t, 'a -> 'b, 'c) t
    | Nil : ('t, 'a, 'a) t
  let (^::) a b = Cons (a,b)
end

module Tuple_out = struct
  type (_, _, _) t =
    | Cons : ('a -> 't) * ('t, 'b, 'c)  t -> ('t, 'a -> 'b, 'c) t
    | Nil : ('t, 'a, 'a) t
  let (^::) a b = Cons (a,b)
end

module Variant_in = struct
  type (_, _) t = Variant: string * ('a, 'constr, 'c) Tuple_in.t  * 'constr -> ('a, 'c) t
end

(** Signature for a driver. Serialization function are on the form [of_XXX] and
    deserialization function are on the form [to_XXX].

    All deserialization functions should only raise [Protocol_error] is the type could not be desrialized.
*)
module type Driver = sig

  (** Serialized type. This type should not be opaque, so it is recommended that
      drivers implement the signature as [Runtime.Driver with type t = ... ]
  *)
  type t

  (** Opaque error type *)
  type error

  (** Exception for protocol errors. The driver should make sure that
      this is the only exception raised when deserializing *)
  exception Protocol_error of error

  (** Construct an error to be raised from a custom parser. *)
  val make_error: ?value: t -> string -> error

  (** Convert an error type to a human readable string *)
  val error_to_string_hum: error -> string

  (** Convert t to a string *)
  val to_string_hum: t -> string

  (** Wrap deserialization function to convert exceptions into an result type *)
  val try_with: (t -> 'v) -> t -> ('v, error) result

  val to_variant: (t, 'a) Variant_in.t list -> t -> 'a
  val of_variant: string -> (t, 'a, t) Tuple_out.t -> 'a

  val to_record:  (t, 'constr, 'b) Record_in.t -> 'constr -> t -> 'b
  val of_record:  (t, 'a, t) Record_out.t -> 'a

  val to_tuple:   (t, 'constr, 'b) Tuple_in.t -> 'constr -> t -> 'b
  val of_tuple:   (t, 'a, t) Tuple_out.t -> 'a

  val to_option:  (t -> 'a) -> t -> 'a option
  val of_option:  ('a -> t) -> 'a option -> t
  val to_ref:     (t -> 'a) -> t -> 'a ref
  val of_ref:     ('a -> t) -> 'a ref -> t
  val to_list:    (t -> 'a) -> t -> 'a list
  val of_list:    ('a -> t) -> 'a list -> t
  val to_array:   (t -> 'a) -> t -> 'a array
  val of_array:   ('a -> t) -> 'a array -> t
  val to_lazy_t:  (t -> 'a) -> t -> 'a lazy_t
  val of_lazy_t:  ('a -> t) -> 'a lazy_t -> t
  val to_int:     t -> int
  val of_int:     int -> t
  val to_int32:   t -> int32
  val of_int32:   int32 -> t
  val to_int64:   t -> int64
  val of_int64:   int64 -> t
  val to_nativeint: t -> nativeint
  val of_nativeint: nativeint -> t
  val to_char:    t -> char
  val of_char:    char -> t
  val to_string:  t -> string
  val of_string:  string -> t
  val to_float:   t -> float
  val of_float:   float -> t
  val to_bool:    t -> bool
  val of_bool:    bool -> t
  val to_unit:    t -> unit
  val of_unit:    unit -> t
end

(** Module contains helper function for serializing and deserializing tuples, records and variants.
    Deserialization functions may raise [Helper.Protocol] exception. It is recommended that the calling functions
    convert this exception into a [Driver.Protocol_exception]
*)
module Helper = struct

  (** Excpetion raised if the type could not be serialized *)
  exception Protocol_error of string

  (**/**)
  module type Lookup = sig
    val of_alist: (string * 'a) list -> string -> 'a option
  end
  module Hashtbl_lookup : Lookup = struct (* 20.22% *)
    let of_alist alist =
      let tbl = Hashtbl.of_alist_exn (module String) alist in
      Hashtbl.find tbl
  end
  module Lookup = Hashtbl_lookup

  let raise_errorf: ('a, unit, string, 'b) format4 -> 'a = fun fmt -> Printf.ksprintf (fun s -> raise (Protocol_error s)) fmt
  (**/**)

  (** Map fields names of a [Record_in] structure *)
  let rec map_record_in: type t a b. (string -> string) -> (t, a, b) Record_in.t -> (t, a, b) Record_in.t = fun field -> function
    | Record_in.Cons ((field_name, to_value_func, default), xs) ->
      Record_in.Cons ((field field_name, to_value_func, default), map_record_in field xs)
    | Record_in.Nil -> Record_in.Nil

  (** {!to_record spec constructor ts} returns the constructed value.
      [ts] is a associative array [(string * t)] list, mapping fields to the deserialized value [t]
      if [strict] is true, an error will be raised if input contains an unknown field.
      If dublicate fields are found in the input, an error is raised
  *)
  let to_record: type t constr b. ?strict:bool -> (t, constr, b) Record_in.t -> constr -> (string * t) list -> b =
    let rec to_alist : type a b c. int -> (a, b, c) Record_in.t -> (string * int) list = fun idx -> function
      | Record_in.Cons ((field, _, _), xs) ->
        (field, idx) :: to_alist (idx + 1) xs
      | Record_in.Nil -> []
    in
    let rec inner: type constr. int -> (t, constr, b) Record_in.t -> constr -> t option array -> b = fun idx ->
      let open Record_in in
      let value_of to_v field default t = match t, default with
        | Some t, _ -> to_v t
        | None, Some d -> d
        | None, None -> raise_errorf "Missing record field: %s" field
      in
      function
      | (Cons ((n1, f1, d1), xs)) ->
        let cont = inner (idx + 1) xs in
        fun constr values ->
          let v1 = value_of f1 n1 d1 values.(idx + 0) in
          cont (constr v1) values

      | Nil -> fun a _values -> a
    in
    fun ?(strict=false) spec constr ->
      let lookup, count =
        let alist = to_alist 0 spec in
        Lookup.of_alist alist, List.length alist
      in
      let f = inner 0 spec constr in

      fun values ->
        let value_array = Array.create ~len:count None in
        List.iter ~f:(fun (field, t) ->
            match lookup field with
            | None when strict -> raise_errorf "Unused field when deserialising record: %s" field
            | None -> ()
            | Some idx -> begin
                match value_array.(idx) with
                | Some _ -> raise_errorf "Multiple fields with the same name: %s" field
                | None -> value_array.(idx) <- Some t
              end
          ) values;
        f value_array

  (** Map fields names of a [Record_out] structure *)
  let rec map_record_out: type t a. (string -> string) -> (t, a, t) Record_out.t -> (t, a, t) Record_out.t =
    fun field ->
    let open Record_out in
    function
      | Cons ((field_name, to_t, default), xs) ->
        Cons ((field field_name, to_t, default), map_record_out field xs)
      | Nil -> Nil

  type 't serialize_record = (string * 't) list -> 't

  (** {!of_record map_f spec} produces a valid deserialisation function for a record type
      The [map_f] function is called to produce the serialized result from a field_name, t association list.
      If [omit_default] is true, then default values are omitted from the output
  *)
  let of_record: type t a t. omit_default:bool -> t serialize_record -> (t, a, t) Record_out.t -> a =
    fun ~omit_default serialize_record ->
    let rec inner: type a. (t, a, t) Record_out.t -> (string * t) list -> a =
      let open Record_out in
      function
      | Cons ((n1, f1, Some d1), xs) when omit_default ->
        begin
          let cont = inner xs in
          fun acc v1 -> match Poly.equal d1 v1 with
            | true -> cont acc
            | false -> cont ((n1, f1 v1) :: acc)
        end
      | Cons ((n1, f1, _), xs) ->
        let cont = inner xs in
        fun acc v1 ->
          cont ((n1, f1 v1) :: acc)
      | Record_out.Nil ->
        fun acc -> serialize_record acc
    in
    fun spec -> inner spec []

  (** {!to_tuple spec tlist} produces a tuple from the serialized values in [tlist] *)
  let rec to_tuple: type t a b. (t, a, b) Tuple_in.t -> a -> t list -> b =
    let open Tuple_in in
    function
    | Cons (f1, xs) -> begin
        let cont = to_tuple xs in
        fun constructor -> function
          | v1 :: ts -> cont (constructor (f1 v1)) ts
          | _ -> raise_errorf "Too few elements when parsing tuple"
      end
    | Nil -> fun a -> begin
      function
      | [] -> a
      | _ -> raise_errorf "Too many elements when parsing tuple"
    end

  type 't serialize_tuple = 't list -> 't
  let of_tuple: type t a. t serialize_tuple -> (t, a, t) Tuple_out.t -> a = fun serialize_tuple ->
    let rec inner: type a. (t, a, t) Tuple_out.t -> t list -> a =
      let open Tuple_out in
      function
      | Cons (f1, Cons (f2, (Cons (f3, (Cons (f4, Nil)))))) ->
        fun acc v1 v2 v3 v4 -> List.rev_append acc [f1 v1; f2 v2; f3 v3; f4 v4] |> serialize_tuple
      | Cons (f1, Cons (f2, (Cons (f3, Nil)))) ->
        fun acc v1 v2 v3 -> List.rev_append acc [f1 v1; f2 v2; f3 v3] |> serialize_tuple
      | Cons (f1, Cons (f2, Nil)) ->
        fun acc v1 v2 -> List.rev_append acc [f1 v1; f2 v2] |> serialize_tuple
      | Cons (f1, Nil) ->
        fun acc v1 -> List.rev_append acc [f1 v1] |> serialize_tuple
      | Nil ->
        fun acc -> List.rev acc |> serialize_tuple
      | Cons (f1, Cons (f2, (Cons (f3, (Cons (f4, Cons (f5, xs))))))) ->
        let cont = inner xs in
        fun acc v1 v2 v3 v4 v5 -> cont (f5 v5 :: f4 v4 :: f3 v3 :: f2 v2 :: f1 v1 :: acc)
    in
    fun spec -> inner spec []

  type 't serialize_variant = string -> 't list -> 't

  (** {!of_variant spec v} serializes v and returns the serialized values
      as a list or map
  *)
  let of_variant: type t. t serialize_variant -> string -> (t, 'a, t) Tuple_out.t -> 'a =
    fun serialize_variant name spec ->
    of_tuple (serialize_variant name) spec

  (** Map field names in all inline records of the spec *)
  let map_constructor_names: (string -> string) -> ('t, 'a) Variant_in.t list -> ('t, 'a) Variant_in.t list =
    fun constructor variant ->
    List.map variant ~f:(fun (Variant_in.Variant (name, spec, constr)) -> Variant_in.Variant (constructor name, spec, constr))

  let to_variant: ('t, 'a) Variant_in.t list -> string -> 't list -> 'a = fun spec ->
    let lookup =
      List.map spec ~f:(fun (Variant_in.Variant (name, spec, constr)) -> name, to_tuple spec constr)
      |> Lookup.of_alist
    in
    fun name args ->
      match lookup name with
      | None -> raise_errorf "Unknown variant name: %s" name
      | Some f -> f args
end
