(* Xml driver for ppx_protocol_conv *)
open StdLabels
open Protocol_conv.Runtime
module Helper = Protocol_conv.Runtime.Helper
type t = Ezxmlm.node

type error = string * t option
exception Protocol_error of error
module StringMap = Map.Make(String)

let make_error ?value msg = (msg, value)

let to_string_hum xml = Ezxmlm.to_string [xml]

let error_to_string_hum: error -> string = function
  | (s, Some t) -> Printf.sprintf "%s. T: '%s'" s (to_string_hum t)
  | (s, None) -> s

(* Register exception printer *)
let () = Printexc.register_printer (function
    | Protocol_error err -> Some (error_to_string_hum err)
    | _ -> None)

let try_with: (t -> 'a) -> t -> ('a, error) result = fun f t ->
  match f t with
  | v -> Ok v
  | exception (Protocol_error e) -> Error e

let raise_errorf t fmt =
  Printf.kprintf (fun s -> raise (Protocol_error (s, t))) fmt

let wrap t f x = match f x with
  | v -> v
  | exception Helper.Protocol_error s -> raise (Protocol_error (s, Some t))

let element name t = Ezxmlm.make_tag name ([], t)

let record_to_xml (assoc:(string * t) list) =
  List.map ~f:(
    function
    | (field, `El (((_,"record"), attrs), xs)) -> [`El ((("",field), attrs), xs)]
    | (field, `El (((_,"variant"), attrs), xs)) -> [`El ((("",field), attrs), xs)]
    | (field, `El (((_,"__option"), attrs), xs)) -> [`El ((("",field), attrs), xs)]
    | (field, `El (((_,_), _), xs)) ->
      List.map ~f:(function
          | `El (((_,_), attrs), xs) -> `El ((("",field), attrs), xs)
          | `Data _ as p -> `El ((("",field), []), [p])
        ) xs
    | (field, e) -> raise_errorf (Some e) "Must be an element: %s" field
  ) assoc
  |> List.concat |> element "record"

let of_variant: string -> (t, 'a, t) Tuple_out.t -> 'a = fun spec ->
  let to_t name args = `El ((("","variant"), []), `Data name :: args) in
  Helper.of_variant to_t spec

let to_variant: (t, 'a) Variant_in.t list -> t -> 'a = fun spec ->
  let f = Helper.to_variant spec in
  function
  | `El (((_, _), _), (`Data s) :: es) as t ->
    wrap t (f s) es
  | `El (((_, name), _), []) as t -> raise_errorf (Some t) "No contents for variant type: %s" name
  | t -> raise_errorf (Some t) "Wrong variant data"

let of_record: type a. (t, a, t) Record_out.t -> a = fun spec ->
  Helper.of_record ~omit_default:false record_to_xml spec

let to_record: (t, 'constr, 'b) Record_in.t -> 'constr -> t -> 'b = fun spec constr ->
  let rec inner: type constr b. (t, constr, b) Record_in.t -> string list = function
    | Record_in.Cons ((field, _, _), xs) -> field :: inner xs
    | Record_in.Nil ->  []
  in
  let fields = inner spec in
  (* Join all elements, including default empty ones *)
  let default_map = List.fold_left fields ~init:StringMap.empty ~f:(fun acc field -> StringMap.add field [] acc) in
  let f = Helper.to_record spec constr in
  function
  | `El (((_,_), _), xs) as t ->
    let args =
      List.fold_left ~init:default_map
        ~f:(fun map -> function
            | `El (((_,name), _), _) as x ->
              let v = match StringMap.find name map with
                | l -> x :: l
                | exception Not_found -> [x]
              in
              StringMap.add name v map
            | _ -> map
          ) xs
      |> (fun map -> StringMap.fold (fun key v acc -> (key, v) :: acc) map [])
      |> List.map ~f:(function
        | (field, [ `El (((_, name), attrs), xs) ]) -> (field, `El ((("",name), (("","record"), "unwrapped") :: attrs), xs))
        | (field, [ `Data _ as d ]) -> (field, d)
        | (field, xs) -> (field, `El ((("",field), []), List.rev xs))
      )
    in
    wrap t f args
  | t -> raise_errorf (Some t) "Expected record element"


let of_tuple: (t, 'a, t) Tuple_out.t -> 'a = fun spec ->
  let rec inner: type a b c. int -> (a, b, c) Tuple_out.t -> (a, b, c) Record_out.t = fun i -> function
    | Tuple_out.Cons (f, xs) ->
      let tail = inner (i+1) xs in
      Record_out.Cons ( (Printf.sprintf "t%d" i, f, None), tail)
    | Tuple_out.Nil -> Record_out.Nil
  in
  of_record (inner 0 spec)

let to_tuple: type constr b. (t, constr, b) Tuple_in.t -> constr -> t -> b = fun spec constr ->
  let rec inner: type a b c. int -> (a, b, c) Tuple_in.t -> (a, b, c) Record_in.t = fun i -> function
    | Tuple_in.Cons (f, xs) ->
      let tail = inner (i+1) xs in
      Record_in.Cons ( (Printf.sprintf "t%d" i, f, None), tail)
    | Tuple_in.Nil -> Record_in.Nil
  in
  let spec = inner 0 spec in
  let f = to_record spec constr in
  fun t -> wrap t f t

let to_option: (t -> 'a) -> t -> 'a option = fun to_value_fun t ->
  match t with
  | (`El (((_,_), ((_,_), "unwrapped") :: _), []))
  | (`El (((_,_), _), []))
  | (`El (((_,_), _), [ `Data "" ] )) ->
    None
  | (`El (((_,_), ((_,_), "unwrapped") :: _), [ (`El ((((_,"__option"), _), _)) as t)]))
  | (`El (((_,"__option"), _), [t]))
  | t ->
    Some (to_value_fun t)

let of_option: ('a -> t) -> 'a option -> t = fun of_value_fun v ->
  match v with
  | None ->
    (`El ((("","__option"), []), []))
  | Some x -> begin
      match of_value_fun x with
      | (`El (((_,"__option"), _), _) as t) ->
        (`El ((("","__option"), []), [t]))
      | t ->
        t
    end

let to_ref: (t -> 'a) -> t -> 'a ref = fun to_value_fun t ->
  let v = to_value_fun t in
  ref v

let of_ref: ('a -> t) -> 'a ref -> t = fun of_value_fun v ->
  of_value_fun !v

let to_result: (t -> 'a) -> (t -> 'b) -> t -> ('a, 'b) result = fun to_ok to_err ->
  let ok = Tuple_in.(Cons (to_ok, Nil)) in
  let err = Tuple_in.(Cons (to_err, Nil)) in
  to_variant Variant_in.[Variant ("Ok", ok, fun v -> Ok v); Variant ("Error", err, fun v -> Error v)]

let of_result: ('a -> t) -> ('b -> t) -> ('a, 'b) result -> t = fun of_ok of_err ->
  let of_ok = of_variant "Ok" Tuple_out.(Cons (of_ok, Nil)) in
  let of_err = of_variant "Error" Tuple_out.(Cons (of_err, Nil)) in
  function
  | Ok ok -> of_ok ok
  | Error err -> of_err err

(** If the given list has been unwrapped since its part of a record, we "rewrap it". *)
let to_list: (t -> 'a) -> t -> 'a list = fun to_value_fun -> function
  | (`El ((_, (_, "unwrapped") :: _), _)) as elm ->
    (* If the given list has been unwrapped since its part of a record, we "rewrap it". *)
    [ to_value_fun elm ]
  | (`El ((_, _), ts)) ->
    Helper.list_map ~f:(fun t -> to_value_fun t) ts
  | e -> raise_errorf (Some e) "Must be an element type"

let of_list: ('a -> t) -> 'a list -> t = fun of_value_fun vs ->
  (`El ((("","l"), []), Helper.list_map ~f:(fun v -> of_value_fun v) vs))

let to_array: (t -> 'a) -> t -> 'a array = fun to_value_fun t ->
  to_list to_value_fun t |> Array.of_list

let of_array: ('a -> t) -> 'a array -> t = fun of_value_fun vs ->
  of_list of_value_fun (Array.to_list vs)

let to_lazy_t: (t -> 'a) -> t -> 'a lazy_t = fun to_value_fun t -> Lazy.from_fun (fun () -> to_value_fun t)

let of_lazy_t: ('a -> t) -> 'a lazy_t -> t = fun of_value_fun v ->
  Lazy.force v |> of_value_fun

let of_value to_string v = (`El ((("","p"), []), [ `Data (to_string v) ]))

let to_value type_name of_string t =
  let s = match t with
    | (`El ((_, _), [])) -> ""
    | (`El ((_, _), [`Data s])) -> s
    | (`El (((_,name), _), _)) as e -> raise_errorf (Some e) "Primitive value expected in node: %s for %s" name type_name
    | `Data _ as e -> raise_errorf (Some e) "Primitive type not expected here when deserializing %s" type_name
  in
  try of_string s with
  | _ -> raise_errorf (Some t) "Failed to convert element to %s." type_name

let to_bool = to_value "bool" bool_of_string
let of_bool = of_value string_of_bool

let to_int = to_value "int" int_of_string
let of_int = of_value string_of_int

let to_int32 = to_value "int32" Int32.of_string
let of_int32 = of_value Int32.to_string

let to_int64 = to_value "int64" Int64.of_string
let of_int64 = of_value Int64.to_string

let to_float = to_value "float" float_of_string
let of_float = of_value string_of_float

let to_string = to_value "string" (fun x -> x)
let of_string = of_value (fun x -> x)

let to_char = to_value "char" (function s when String.length s = 1 -> s.[0]
                                      | s -> raise_errorf None "Expected char, got %s" s)
let of_char = of_value (fun c -> (String.make 1 c))

let to_bytes = to_value "bytes" Bytes.of_string
let of_bytes = of_value Bytes.to_string

let to_unit = to_value "unit" (function "()" -> () | _ -> raise_errorf None "Expected unit")
let of_unit = of_value (fun () -> "()")

let to_nativeint = to_value "nativeint" Nativeint.of_string
let of_nativeint = of_value Nativeint.to_string

let of_xmlm_exn: t -> t =
  function
  | (`El ((_v, (_, "unwrapped") :: ((_, "__name"), v') :: xs), d)) -> (`El ((("", v'), xs), d))
  | (`El ((v, (_, "unwrapped") :: xs), d)) -> (`El ((v, xs), d))
  | (`El ((_v, ((_, "__name"), v') :: xs), d)) -> (`El ((("", v'), xs), d))
  | x -> x

let of_xmlm t = Ok (of_xmlm_exn t)
let to_xmlm: t -> t = function
  | (`El ((v, attrs), d)) -> (`El ((v, (("", "__name"), snd v) :: attrs), d))
  | v -> v
