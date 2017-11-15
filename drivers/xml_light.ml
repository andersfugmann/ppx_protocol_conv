(* Xml driver for ppx_deriving_protocol *)
open Base
open Deriving_protocol.Runtime
type t = Xml.xml list
type flags = unit

(* Always return a list of elements *)
let rec member: string -> t -> t = fun field -> function
  | (Xml.Element(name, _, _) as x) :: xs when String.equal name field -> x :: (member field xs)
  | _ :: xs -> member field xs
  | [] -> []

let element name t = [ Xml.Element (name, [], t) ]

let to_record: type a b. flags:flags option -> (t, a, b) structure -> a -> t -> b =
  let rec inner: type a b. (t, a, b) structure -> a -> t -> b = function
    | Cons ((field, to_value_func), xs) ->
      let cont = inner xs in
      fun constr t ->
        let v = member field t |> to_value_func in
        cont (constr v) t
    | Nil -> fun a _t -> a
  in
  fun ~flags:_ spec ->
    let f = inner spec in
    fun constr -> function [ Xml.Element (_, _, t) ] -> f constr t
                         | _ -> failwith "Not a record superstruture"

let of_record: flags:flags option -> (string * t) list -> t = fun ~flags:_ assoc ->
  List.concat_map ~f:(
    fun (name, es) -> List.map ~f:(
        (* If a list was a list of Elements, it would be the same. *)
        function Xml.Element (_name, attributes, data) -> Xml.Element(name, attributes, data)
               | Xml.PCData _ as e -> Xml.Element (name, [], [e])
                 (* failwith ("Record elements cannot be pc data elements: " ^ s) *)
      ) es
  ) assoc |> element "record"

let _name = function Xml.Element(name, _, _) -> name
                   | PCData _ -> "<pcdata>"

let to_tuple = to_record

let of_tuple = of_record

let to_option: flags:flags option -> (t -> 'a) -> t -> 'a option = fun ~flags:_ to_value_fun -> function
  | []
  | [Xml.Element (_, _, [])]
  | [Xml.Element (_, _, [PCData ""])] -> None
  | t -> Some (to_value_fun t)

let of_option: flags:flags option -> ('a -> t) -> 'a option -> t = fun ~flags:_ of_value_fun -> function
  | None -> []
  | Some x -> of_value_fun x

let to_list: flags:flags option -> (t -> 'a) -> t -> 'a list = fun ~flags:_ to_value_fun t ->
  List.map ~f:(fun t -> to_value_fun [t]) t

let of_list: flags:flags option -> ('a -> t) -> 'a list -> t = fun ~flags:_ of_value_fun vs ->
  List.concat_map ~f:(fun v -> of_value_fun v) vs

let of_value fmt = Base.Printf.ksprintf (fun s -> [ Xml.Element ("primitive", [], [ Xml.PCData s ]) ]) fmt

let to_value fmt : t -> 'a = function
  | Xml.Element(_, _, [PCData s]) :: []  -> Caml.Scanf.sscanf s fmt (fun i -> i)
  | Xml.Element(name, _, _) :: _ -> failwith ("Primitive value in node expected. " ^ name)
  | Xml.PCData _ :: _ -> failwith "Primitive type not expected here"
  | [] -> failwith ("No element:" ^ Caml.string_of_format fmt)

let to_int ~flags:_ = to_value "%d"
let of_int ~flags:_ = of_value "%d"

let to_string ~flags:_ = to_value "%s"
let of_string ~flags:_ = of_value "%s"

let to_float ~flags:_ = to_value "%f"
let of_float ~flags:_ = of_value "%f"

let to_bool ~flags:_ = to_value "%b"
let of_bool ~flags:_ = of_value "%b"

let to_unit ~flags:_ = function [ Xml.PCData "" ] -> ()
                            | _ -> failwith "unit expected"

let of_unit ~flags:_ () = [ Xml.PCData "" ]


(* Variants without arguments should be encoded as a string.
   Variant with arguments should be encoded as a list
*)
