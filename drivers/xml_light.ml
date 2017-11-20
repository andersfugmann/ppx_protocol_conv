(* Xml driver for ppx_deriving_protocol *)
open Base
open Deriving_protocol.Runtime
type t = Xml.xml list
type 'a flags = 'a no_flags


let rec element_to_map m = function
  | (Xml.Element(name, _, _) as x) :: xs ->
    let m = Map.add_multi m ~key:name ~data:x in
    element_to_map m xs
  | _ :: xs -> element_to_map m xs
  | [] -> m

let element name t = [ Xml.Element (name, [], t) ]

let of_variant f t =
  let (s, ts) = f t in
  [ Xml.Element("v", [], Xml.PCData s :: (List.concat ts)) ]

let to_variant (f: (string * t list) -> 'a) = function
  | [ Xml.Element(_, _, Xml.PCData s :: es) ] ->
    f (s, (List.map ~f:(fun e -> [e]) es))
  | _ -> failwith "Wrong variant data"

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
        let v = Map.find t field
                |> Option.value ~default:[]
                |> List.rev
                |> to_value_func
        in
        cont (constr v) t
    | Nil -> fun a _t -> a
  in
  let f = inner spec in
  fun constr -> function
    | [ Xml.Element (_, _, t) ] ->
      let m = Map.Using_comparator.empty ~comparator:String.comparator in
      f constr (element_to_map m t)
    | _ -> failwith "Not a record superstruture"

let of_record: (string * t) list -> t = fun assoc ->
  List.concat_map ~f:(
    fun (name, es) -> List.map ~f:(
        function
        | Xml.Element (_name, attributes, data) ->
          Xml.Element(name, attributes, data)
        | Xml.PCData _ as e -> Xml.Element (name, [], [e])
      ) es
  ) assoc |> element "record"


let to_tuple = to_record

let of_tuple = of_record

let to_option: (t -> 'a) -> t -> 'a option = fun to_value_fun -> function
  | []
  | [Xml.Element (_, _, [])]
  | [Xml.Element (_, _, [PCData ""])] -> None
  | t -> Some (to_value_fun t)

let of_option: ('a -> t) -> 'a option -> t = fun of_value_fun -> function
  | None -> []
  | Some x -> of_value_fun x

let to_list: (t -> 'a) -> t -> 'a list = fun to_value_fun t ->
  List.map ~f:(fun t -> to_value_fun [t]) t

let of_list: ('a -> t) -> 'a list -> t = fun of_value_fun vs ->
  List.concat_map ~f:(fun v -> of_value_fun v) vs

let of_value fmt = Base.Printf.ksprintf (fun s -> [ Xml.Element ("p", [], [ Xml.PCData s ]) ]) fmt

let to_value fmt : t -> 'a = function
  | Xml.Element(_, _, [PCData s]) :: []  -> Caml.Scanf.sscanf s fmt (fun i -> i)
  | Xml.Element(name, _, _) :: _ -> failwith ("Primitive value in node expected. " ^ name)
  | Xml.PCData _ :: _ -> failwith "Primitive type not expected here"
  | [] -> failwith ("No element:" ^ Caml.string_of_format fmt)

let to_int = to_value "%d"
let of_int = of_value "%d"

let to_string = to_value "%s"
let of_string = of_value "%s"

let to_float = to_value "%f"
let of_float = of_value "%f"

let to_bool = to_value "%b"
let of_bool = of_value "%b"

let to_unit = function [ Xml.PCData "" ] -> ()
                              | _ -> failwith "unit expected"
let of_unit () = [ Xml.PCData "" ]


let to_int32 = to_value "%ld"
let of_int32 = of_value "%ld"

let to_int64 = to_value "%Ld"
let of_int64 = of_value "%Ld"
