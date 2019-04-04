open Base

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
  type  (_, _) t =
    | Tuple:  ('a, 'b, 'c) Tuple_in.t  * 'b -> ('a, 'c) t
    | Record: ('a, 'b, 'c) Record_in.t * 'b -> ('a, 'c) t
end

module Variant_out = struct
  type ('a, 'b, 'c) t =
    | Tuple of ('a, 'b, 'c) Tuple_out.t
    | Record of ('a, 'b, 'c) Record_out.t
end

module type Driver = sig
  type t
  exception Protocol_error of string * t option
  val to_string_hum: t -> string

  val to_variant: (string * (t, 'c) Variant_in.t) list -> t -> 'c
  val of_variant: string -> (t, 'a, t) Variant_out.t -> 'a
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


module Helper = struct
  exception Protocol_error of string

  (**/**)
  type 'a stringmap = (string, 'a, String.comparator_witness) Map.t
  let raise_errorf: ('a, unit, string, 'b) format4 -> 'a = fun fmt -> Printf.ksprintf (fun s -> raise (Protocol_error s)) fmt
  (**/**)

  type 'a variant = Record of (string * 'a) list | Tuple of 'a list | Nil

  (** Map fields names of a {Record_in} structure *)
  let rec map_record_in: type t a b. field:(string -> string) -> (t, a, b) Record_in.t -> (t, a, b) Record_in.t = fun ~field -> function
    | Record_in.Cons ((field_name, to_value_func, default), xs) ->
      Record_in.Cons ((field field_name, to_value_func, default), map_record_in ~field xs)
    | Record_in.Nil -> Record_in.Nil

  (** {to_record spec constructor ts} returns the constructed value.
       [ts] is a associative array of (fieldname, t)
      If default is set, then this value is used if the field name is not in [ts]
  *)
  let to_record: type t constr b. ?strict:bool -> ?default:t -> (t, constr, b) Record_in.t -> constr -> (string * t) list -> b = fun ?(strict=false) ?default ->
    let rec inner: type constr. (t, constr, b) Record_in.t -> constr -> t stringmap -> b = function
      | Record_in.Cons ((field, to_value_func, Some default), xs) ->
        let cont = inner xs in
        fun constr map ->
          let t = ref None in
          let map = Map.change map field ~f:(fun a -> t := a; None) in
          let v = match !t with
            | Some t -> to_value_func t
            | None -> default
          in
          cont (constr v) map
      | Record_in.Cons ((field, to_value_func, None), xs) ->
        let cont = inner xs in
        fun constr map ->
          let t = ref None in
          let map = Map.change map field ~f:(fun a -> t := a; None) in
          let v = match !t with
            | Some t -> to_value_func t
            | None -> begin
                match default with
                | None -> raise_errorf "Field not found: %s" field
                | Some t -> to_value_func t
              end
          in
          cont (constr v) map
      | Record_in.Nil when strict -> fun a map -> begin
          match Map.is_empty map with
          | true -> a
          | false -> raise_errorf "Superfluous fields for map: [%s]" (Map.keys map |> String.concat ~sep:"; ")
        end
      | Record_in.Nil -> fun a _map -> a
    in
    fun spec constr ->
      let f = inner spec constr in
      fun elements ->
        let map = List.fold_left ~init:(Map.empty (module String))
            ~f:(fun acc (field, t) ->
                match Map.add acc ~key:field ~data:t with
                | `Ok map -> map
                | `Duplicate -> raise_errorf "Duplicate fields found: %s" field
          ) elements
        in
        f map

  (** Map fields names of a {Record_out} structure *)
  let rec map_record_out: type t a. field:(string -> string) -> (t, a, t) Record_out.t -> (t, a, t) Record_out.t = fun ~field -> function
    | Record_out.Cons ((field_name, to_t, default), xs) ->
      Record_out.Cons ((field field_name, to_t, default), map_record_out ~field xs)
    | Record_out.Nil -> Record_out.Nil

  let rec of_record: type t a. omit_default:bool -> ((string * t) list -> t) -> (t, a, t) Record_out.t -> (string * t) list -> a = fun ~omit_default map -> function
    | Record_out.Cons ((field, to_t, default), xs) ->
      let cont = of_record ~omit_default map xs in
      let f = match omit_default, default with
        | true, Some d -> begin
            fun acc -> function
              | v when Poly.equal v d -> cont acc
              | v -> cont ((field, to_t v) :: acc)
          end
        | _, _ -> fun acc v -> cont ((field, to_t v) :: acc)
      in
      f
    | Record_out.Nil ->
      fun acc -> map acc

  (** {of_record map_f spec} produces a valid deserialisation function for a record type
      The [map_f] function is called to produce the serialised result from a field_name, t association list.
      If [omit_default] is true, then default values are omitted from the output
  *)
  let of_record ?(omit_default=false) map_f spec = of_record ~omit_default map_f spec []


  (** {to_tuple spec tlist} produces a tuple from the serialized values in [tlist] *)
  let rec to_tuple: type t a b. (t, a, b) Tuple_in.t -> a -> t list -> b =
    function
    | Tuple_in.Cons (to_value_func, xs) ->
      let cont = to_tuple xs in
      fun constructor -> begin function
          | t :: ts ->
            let v = to_value_func t in
            cont (constructor v) ts
          | [] -> raise_errorf "Too few elements when parsing tuple"
        end
    | Tuple_in.Nil -> fun a -> function
      | [] -> a
      | _ -> raise_errorf "Too many elements when parsing tuple"

  let rec of_tuple: type t a. (t list -> t) -> (t, a, t) Tuple_out.t -> t list -> a = fun map -> function
    | Tuple_out.Cons (to_t, xs) ->
      let cont = of_tuple map xs in
      fun acc v ->
        cont (to_t v :: acc)
    | Tuple_out.Nil ->
      fun acc -> List.rev acc |> map

  (** {of_tuple spec} prduces a list of serialized values from the given tuple [v] *)
  let of_tuple map_f spec = of_tuple map_f spec []

  (** Map field names in all inline records of the spec *)
  let map_variant_out: field:(string -> string) -> ('a, 'b, 'c) Variant_out.t -> ('a, 'b, 'c) Variant_out.t = fun ~field -> function
    | Variant_out.Record spec -> Variant_out.Record (map_record_out ~field spec)
    | Variant_out.Tuple spec -> Variant_out.Tuple spec

  (** {of_variant spec v} serialises v and returns the serialized values as a list or map *)
  let of_variant: type t a. ?omit_default:bool -> (t variant -> t) -> (t, a, t) Variant_out.t -> a = fun ?(omit_default=false) map_f -> function
    | Variant_out.Record spec ->
      of_record ~omit_default (fun x -> Record x |> map_f) spec
    | Variant_out.Tuple Tuple_out.Nil -> of_tuple (fun _ -> map_f Nil) Tuple_out.Nil
    | Variant_out.Tuple spec ->
      of_tuple (fun x -> Tuple x |> map_f) spec

  (** Map field names in all inline records of the spec *)
  let map_variant_in: field:(string -> string) -> constructor:(string -> string) -> (string * ('a,'b) Variant_in.t) list -> (string * ('a,'b) Variant_in.t) list =
    fun ~field ~constructor variants ->
    List.map ~f:(fun (name, spec) ->
        let name = constructor name in
        let spec = match spec with
          | Variant_in.Record (spec, v) -> Variant_in.Record (map_record_in ~field spec, v)
          | Variant_in.Tuple (spec, v) -> Variant_in.Tuple (spec, v)
        in
        name, spec
      ) variants

  let to_variant: type t c. ?strict:bool -> (t -> (string * t) list option) -> (string * (t, c) Variant_in.t) list -> string -> t list -> c = fun ?strict to_alist spec ->
    let map_variant_constr: type c. (t, c) Variant_in.t -> t list -> c = function
      | Variant_in.Record (spec, constructor) ->
        begin
          let f = to_record ?strict spec constructor in
          function
          | [t] ->
            let v = match to_alist t with
              | Some alist -> alist
              | None -> raise_errorf "Expected map when deserialising variant of record"
            in
            f v
          | _ -> raise_errorf "Expected exactly one alist argument when deserialising variant of record"
        end
      | Variant_in.Tuple (Tuple_in.Nil, constructor) ->
        begin
          function
          | [] -> constructor
          | _ -> failwith "Too many arguments when deserialising variant with no arguments"
        end
      | Variant_in.Tuple (spec, constructor) ->
        begin
          let f = to_tuple spec constructor in
          fun t -> f t
        end
    in
    let map = List.fold_left ~init:(Map.empty (module String)) ~f:(fun acc (name, spec) ->
        Map.add_exn acc ~key:name ~data:(map_variant_constr spec)
      ) spec
    in
    fun name t ->
      match Map.find map name with
      | Some f -> f t
      | None -> raise_errorf "Unknown constructor name: %s" name
end
