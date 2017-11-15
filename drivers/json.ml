open Base
open Deriving_protocol
type t = Yojson.Safe.json
type flags = [ `Mangle of (string->string) ]

(* Missing int32 and int64 (maybe other primitive types) *)

exception Dubplicate_key of string * string

(* Convert a_bcd_e_ to aBcdE *)
let mangle (s : string) =
  let rec inner : char list -> char list = function
    | '_' :: c :: cs -> (Char.uppercase c) :: (inner cs)
    | '_' :: [] -> []
    | c :: cs -> c :: (inner cs)
    | [] -> []
  in
  String.to_list s |> inner |> String.of_char_list

let rec to_record: type a b. flags:flags option -> (t, a, b) Runtime.structure -> a -> t -> b = fun ~flags ->
  let open Runtime in
  let field_func x = match flags with
    | None -> x
    | Some `Mangle f -> f x
  in
  function
  | Cons ((field, to_value_func), xs) ->
    let field_name = field_func field in
    let cont = to_record ~flags xs in
    fun constr t ->
      let v = Yojson.Safe.Util.member field_name t |> to_value_func in
      cont (constr v) t
  | Nil -> fun a _t -> a

let of_record: flags:flags option -> (string * t) list -> t = fun ~flags assoc ->
  let assoc = match flags with
    | None -> assoc
    | Some `Mangle mangle ->
        List.map ~f:(fun (k, v) -> (mangle k, v)) assoc
  in
  `Assoc assoc

let rec to_tuple: type a b. flags:flags option -> (t, a, b) Runtime.structure -> a -> t -> b =
  fun ~flags ->
    let open Runtime in
    function
    | Cons ((_field, to_value_func), xs) ->
      fun constructor t ->
        let l = Yojson.Safe.Util.to_list t in
        let v = to_value_func (List.hd_exn l) in
        to_tuple ~flags xs (constructor v) (`List (List.tl_exn l))
    | Nil -> fun a _t -> a

let of_tuple ~flags:_ t = `List (List.map ~f:snd t)

let to_option: flags:flags option -> (t -> 'a) -> t -> 'a option = fun ~flags:_ to_value_fun -> function
  | `Null -> None
  | x -> Some (to_value_fun x)
let of_option: flags:flags option -> ('a -> t) -> 'a option -> t = fun ~flags:_ of_value_fun -> function
  | None -> `Null
  | Some x -> of_value_fun x

let to_list: flags:flags option -> (t -> 'a) -> t -> 'a list = fun ~flags:_ to_value_fun t ->
  List.map ~f:to_value_fun (Yojson.Safe.Util.to_list t)
let of_list: flags:flags option -> ('a -> t) -> 'a list -> t = fun ~flags:_ of_value_fun v ->
  `List (List.map ~f:of_value_fun v)

let to_int ~flags:_ t = Yojson.Safe.Util.to_int t
let of_int ~flags:_ (i:int) : t = `Int i

let to_string ~flags:_ t = Yojson.Safe.Util.to_string t
let of_string ~flags:_ s = `String s

let to_float ~flags:_ t = Yojson.Safe.Util.to_float t
let of_float ~flags:_ s = `Float s

let to_bool ~flags:_ t = Yojson.Safe.Util.to_bool t
let of_bool ~flags:_ b= `Bool b

let to_unit ~flags t = to_tuple ~flags Runtime.Nil () t
let of_unit ~flags () = of_tuple ~flags []
