(* Xml driver for ppx_protocol_conv *)
open Base
open Protocol_conv.Runtime
type t = Xml.xml
type 'a flags = 'a no_flags

exception Protocol_error of string * t
(* Register exception printer *)
let () = Caml.Printexc.register_printer
    (function Protocol_error (s, t) -> Some (s ^ ": " ^ (Xml.to_string t))
            | _ -> None)

let raise_errorf t fmt =
  Caml.Printf.kprintf (fun s -> raise (Protocol_error (s, t))) fmt

(* We are actually able to determine if we should inline by looking at the node name.
   Alternativly, we need to wrap records into yet another level *)
let rec element_to_map m = function
  | (Xml.Element(name, _, _) as x) :: xs ->
    let m = Map.add_multi m ~key:name ~data:x in
    element_to_map m xs
  | _ :: xs -> element_to_map m xs
  | [] -> m

let element name t = Xml.Element (name, [], t)

let of_variant: (('a -> string * t list) -> 'a -> t) flags = fun destruct t ->
  let (s, ts) = destruct t in
  Xml.Element("variant", [], Xml.PCData s :: ts)

let to_variant: ((string * t list -> 'a) -> t -> 'a) flags = fun constr -> function
  | Xml.Element(_, _, Xml.PCData s :: es) -> constr (s, es)
  | Xml.Element(name, _, []) as d -> raise_errorf d "No contents for variant type: %s" name
  | d -> raise_errorf d "Wrong variant data"

(* Records could be optimized by first creating a map of existing
   usable labels -> id's (at startup). Then map the input data to an
   array of lists (mutable). Then do the decoding. That would be O(n
   log n) (n = fields), but its almost the same as the current, which
   does two lookups: O(2 * n log n) = O(n log n) where n is the number
   of fields in the input data. There is hardly anypoint to
   that. Although it would be fun to create.
*)
let to_record: type a b. (t, a, b) structure -> a -> t -> b = fun spec ->
  let rec inner: type a b. (t, a, b) structure -> a -> 't -> b = function
    | Cons ((field, to_value_func), xs) ->
      let cont = inner xs in
      fun constr t ->
        let values =
          Map.find t field
          |> Option.value ~default:[]
          |> List.rev
        in
        let arg = match values with
          | [ Xml.Element (name, _, xs) ] -> Xml.Element (name, ["record", "unwrapped"], xs)
          | [ Xml.PCData _ as d ] -> d
          | xs -> Xml.Element (field, [], xs)
        in
        let v = to_value_func arg
        in
        cont (constr v) t
    | Nil -> fun a _t -> a
  in
  let f = inner spec in
  fun constr -> function
    | Xml.Element (_, _, t) ->
      let m = Map.Using_comparator.empty ~comparator:String.comparator in
      f constr (element_to_map m t)
    | e -> raise_errorf e "Not a record superstruture"

(* A : int list -> "a", Element("l" , [], Element list)  *)
let of_record: (string * t) list -> t = fun assoc ->
  List.concat_map ~f:(
    function
    | (field, Xml.Element ("record", attrs, xs)) -> [Xml.Element (field, attrs, xs)]
    | (field, Xml.Element ("variant", attrs, xs)) -> [Xml.Element (field, attrs, xs)]
    | (field, Xml.Element (_, _, xs)) ->
      List.map ~f:(function
          | Xml.Element(_, attrs, xs) -> Xml.Element(field, attrs, xs)
          | x -> Xml.Element(field, [], [x])
        ) xs
    | (field, e) -> raise_errorf e "Must be an element: %s" field
  ) assoc |> element "record"


let to_tuple = to_record

let of_tuple = of_record

let to_option: (t -> 'a) -> t -> 'a option = fun to_value_fun -> function
  | Xml.Element (_, _, [])
  | Xml.Element (_, _, [PCData ""]) -> None
  | t -> Some (to_value_fun t)

let of_option: ('a -> t) -> 'a option -> t = fun of_value_fun -> function
  | None -> Xml.Element ("o", [], [])
  | Some x -> of_value_fun x

(** If the given list has been unwrapped since its part of a record, we "rewrap it". *)
let to_list: (t -> 'a) -> t -> 'a list = fun to_value_fun -> function
  | Xml.Element (_, [_, "unwrapped"], _) as elm ->
    (* If the given list has been unwrapped since its part of a record, we "rewrap it". *)
    [ to_value_fun elm ]
  | Xml.Element (_, _, ts) ->
    List.map ~f:(fun t -> to_value_fun t) ts
  | e -> raise_errorf e "Must be an element type"

let of_list: ('a -> t) -> 'a list -> t = fun of_value_fun vs ->
  Xml.Element("l", [], List.map ~f:(fun v -> of_value_fun v) vs)

let to_lazy_t: (t -> 'a) -> t -> 'a lazy_t = fun to_value_fun t -> Lazy.from_fun (fun () -> to_value_fun t)

let of_lazy_t: ('a -> t) -> 'a lazy_t -> t = fun of_value_fun v ->
  Lazy.force v |> of_value_fun


let of_value to_string v = Xml.Element ("p", [], [ Xml.PCData (to_string v) ])
let to_value type_name of_string = function
  | Xml.Element(_, _, [PCData s]) -> of_string s
  | Xml.Element(name, _, _) as e -> raise_errorf e "Primitive value expected in in node: %s for %s" name type_name
  | Xml.PCData _ as e -> raise_errorf e "Primitive type not expected here when deserializing %s" type_name

let to_bool = to_value "bool" Bool.of_string
let of_bool = of_value Bool.to_string

let to_int = to_value "int" Int.of_string
let of_int = of_value Int.to_string

let to_int32 = to_value "int32" Int32.of_string
let of_int32 = of_value Int32.to_string

let to_int64 = to_value "int64" Int64.of_string
let of_int64 = of_value Int64.to_string

let to_float = to_value "float" Float.of_string
let of_float = of_value Float.to_string

let to_string = to_value "string" String.of_string
let of_string = of_value String.to_string

let to_unit = function Xml.Element (_, _, [])
                     | Xml.Element (_, _, [ PCData "" ]) -> ()
                     | e -> raise_errorf e "Unit must be an empty element"
let of_unit () = Xml.Element ("u", [], [])

let t_of_xml_light t = t
let t_to_xml_light t = t
