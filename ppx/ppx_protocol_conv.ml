open Ppxlib
open Ast_builder.Default
open !Printf
open Base

type t = {
  driver: longident;
  field_key:
    (label_declaration, string) Attribute.t;
  constr_key:
    (constructor_declaration, string) Attribute.t;
  constr_name:
    (constructor_declaration, string) Attribute.t;
  variant_key:
    (row_field, string) Attribute.t;
  variant_name:
    (row_field, string) Attribute.t;
  field_default:
    (label_declaration, expression) Attribute.t;
}

(* In variants, dont encode as tuple.... *)
let (^^) = Caml.(^^)
let raise_errorf ?loc fmt = Location.raise_errorf ?loc ("ppx_protocol_conv: " ^^ fmt)

let debug = false
let debug fmt = match debug with
  | true -> eprintf (fmt ^^ "\n%!")
  | false -> ifprintf Stdio.stderr fmt [@@warning "-32"]

let string_of_ident_loc { loc; txt } =
  let rec inner = function
    | Lident s -> s
    | Ldot (i, s) -> inner i ^ "." ^ s
    | Lapply _  -> raise_errorf ~loc "lapply???"
  in
  { loc; txt=inner txt }

let _longident_loc_of_string ~loc s =
  { txt = Lident s; loc }

let pexp_ident_string_loc { loc; txt } =
  pexp_ident ~loc {loc; txt = Lident txt}

let rec _string_of_ident = function
  | Lident s -> s
  | Ldot (i, s) -> _string_of_ident i ^ "." ^ s
  | Lapply _  -> raise_errorf "lapply???"

let ident_of_string_loc { loc; txt} = { loc; txt=Lident txt }

let driver_func t ~loc name =
  let func = pexp_ident ~loc { loc; txt = Ldot (t.driver, name) } in
  [%expr ([%e func])]

(** Concatinate the list of expressions into a single expression using
   list concatenation *)
let list_expr ~loc l =
  List.fold_right ~init:[%expr []] ~f:(fun hd tl -> [%expr [%e hd] :: [%e tl]]) l

let slist_expr ~loc l =
  List.fold_right ~init:[%expr Nil] ~f:(fun hd tl -> [%expr Cons ([%e hd], [%e tl])]) l

let ident_of_module ~loc = function
  | Some { pmod_desc = Pmod_ident { txt; _ }; _ } -> txt
  | Some _ -> raise_errorf ~loc "must be a module identifier"
  | None -> raise_errorf ~loc "~driver argument missing"

(** Test is a type is considered primitive *)
let is_primitive_type = function
  | "string" | "int" | "int32" | "int64" | "nativeint" | "float" | "bool" | "char" | "unit" -> true
  | _ -> false

(** Test if the type is a type modifier *)
let is_meta_type = function
  | "option" | "array" | "list" | "lazy_t" | "ref" -> true
  | _ -> false

let module_name ?loc = function
  | Lident s -> String.uncapitalize s
  | Ldot (_, s) -> String.uncapitalize s
  | Lapply _ -> raise_errorf ?loc "lapply???"

let _default_case ~loc =
  case ~lhs:[%pat? (s, _)]
    ~guard:None
    ~rhs:[%expr failwith ("Unknown variant or arity error: " ^ s)]

let protocol_ident dir driver { loc; txt } =
  (* Match the name of the type *)
  let driver_name = module_name ~loc driver in
  let txt = match txt with
    | Lident "t" -> Lident (sprintf "%s_%s" dir driver_name)
    | Lident name -> Lident (sprintf "%s_%s_%s" name dir driver_name)
    | Ldot (l, "t") -> Ldot (l, sprintf "%s_%s" dir driver_name)
    | Ldot (l, name) -> Ldot (l, sprintf "%s_%s_%s" name dir driver_name)
    | Lapply _  -> raise_errorf ~loc "lapply???"
  in
  pexp_ident ~loc { loc; txt }

(** Test that all label names are distict after mapping
    This function will raise an error is a conflict is found
*)
let location_of_attrib t name (attribs:attributes) =
  let prefix = module_name t.driver in
  let has_name s = String.equal s name || String.equal s (sprintf "%s.%s" prefix name) in
  List.find_map_exn
    ~f:(function ({ loc=_; txt}, Parsetree.PStr [{pstr_loc; _}]) when has_name txt -> Some pstr_loc
               | _ -> None
      ) attribs

let row_loc = function
  | Rtag (sloc, _, _, _) -> sloc.loc
  | Rinherit typ -> typ.ptyp_loc

let get_variant_name t row =
  match Attribute.get t.variant_name row, Attribute.get t.variant_key row with
  | Some name, None -> Some name
  | None, Some name -> Some name
  | Some _, Some _ -> raise_errorf ~loc:(row_loc row)  "Both 'key' and 'name' attributes supplied. Use of @@key is deprecated - use @@name instead"
  | None, None -> None

let get_constr_name t constr =
  match Attribute.get t.constr_name constr, Attribute.get t.constr_key constr with
  | Some name, None -> Some name
  | None, Some name -> Some name
  | Some _, Some _ -> raise_errorf ~loc:(constr.pcd_loc) "Both 'key' and 'name' attributes supplied. Use of @@key is deprecated - use @@name instead"
  | None, None -> None


(** Test that all constructor names are distict after mapping
    This function will raise an error is a conflict is found
*)
let test_constructor_mapping t constrs =
  let base, mapped = List.partition_map ~f:(fun constr ->
      match get_constr_name t constr with
      | Some name when String.equal constr.pcd_name.txt name -> `Fst name
      | Some name -> `Snd (name, constr.pcd_attributes)
      | None -> `Fst constr.pcd_name.txt
    ) constrs
  in
  let _: string list = List.fold_left ~init:base
      ~f:(fun acc -> function
          | (name, attrs) when List.mem ~equal:String.equal acc name ->
            let loc = location_of_attrib t "key" attrs in (* Should use the name of the attribute *)
            raise_errorf ~loc "Mapped constructor name already in use: %s" name
          | (name, _) -> name :: acc
        ) mapped
  in
  ()

let test_row_mapping t rows =
  let base, mapped = List.partition_map ~f:(fun row ->
      let (row_name, attrs) = match row with
        | Rinherit _ -> raise_errorf "Inherited polymorphic variant types not supported"
        | Rtag (name, attrs, _, _) -> name, attrs
      in
      match get_variant_name t row with
      | Some name when String.equal row_name.txt name -> `Fst name
      | Some name -> `Snd (name, attrs)
      | None -> `Fst row_name.txt
    ) rows
  in
  let _: string list = List.fold_left ~init:base
      ~f:(fun acc -> function
          | (name, attrs) when List.mem ~equal:String.equal acc name ->
            let loc = location_of_attrib t "key" attrs in (* Should use the name of the attribute *)
            raise_errorf ~loc "Mapped constructor name already in use: %s" name
          | (name, _) -> name :: acc
        ) mapped
  in
  ()


(** Serialization expression for a given type *)
let rec serialize_expr_of_type_descr t ~loc = function
  | Ptyp_constr ({ txt=Lident ident; loc }, [ct]) when is_meta_type ident ->
    let to_p = serialize_expr_of_type_descr t ~loc ct.ptyp_desc in
    pexp_apply ~loc (driver_func ~loc t ("of_" ^ ident)) [Nolabel, to_p]

  | Ptyp_constr ({ txt=Lident ident; loc=_ }, _) when is_meta_type ident ->
    raise_errorf ~loc "Unsupported type descr containing list of sub-types"

  | Ptyp_constr ({ txt=Lident s; loc }, _) when is_primitive_type s ->
    driver_func t ~loc ("of_" ^ s)

  | Ptyp_constr (ident, cts) ->
    (* Call recursivly to for app type parameters *)
    let args =
      List.map ~f:(fun { ptyp_desc; ptyp_loc; _ } ->
          serialize_expr_of_type_descr t ~loc:ptyp_loc ptyp_desc) cts
      |> List.map ~f:(fun expr -> (Nolabel, expr))
    in
    let func = protocol_ident "to" t.driver ident in
    pexp_apply ~loc func args

  | Ptyp_tuple cts -> begin
      let spec =
        List.map cts ~f:(fun ct -> serialize_expr_of_type_descr t ~loc ct.ptyp_desc)
        |> slist_expr ~loc
      in
      let ids = List.mapi cts ~f:(fun i _ -> sprintf "t%d" i) in
      let args = List.map ids ~f:(fun id -> Nolabel, pexp_ident ~loc { loc; txt=Lident id } ) in
      let patt =
        List.map ids ~f:(fun id -> ppat_var ~loc { loc; txt=id })
        |> ppat_tuple ~loc
      in
      [%expr
        let _of_tuple =
          [%e driver_func t ~loc "of_tuple"] Protocol_conv.Runtime.Tuple_out.([%e spec])
        in
        fun [%p patt] -> [%e pexp_apply ~loc [%expr _of_tuple] args]
      ]
    end
  | Ptyp_poly _      -> raise_errorf ~loc "Polymorphic variants not supported"
  | Ptyp_variant (rows, _closed, None) ->
    (*
       type t = [`A | `B of int]

       let to_json =
         let _A_of_tuple = Json.of_variant "A" (Protocol_conv.Runtime.Variant_out.Tuple
                (let open Protocol_conv.Runtime.Tuple_out in Nil))
         and _B_of_tuple = Json.of_variant "B" (Protocol_conv.Runtime.Variant_out.Tuple
                (let open Protocol_conv.Runtime.Tuple_out in Cons (Json.of_int, Nil)))
         in
         function `A -> _A_of_tuple
                | `B c0 -> _B_of_tuple c0
    *)
    test_row_mapping t rows;
    let mk_pattern core_types =
      List.mapi ~f:(fun i (core_type:core_type) ->
          ppat_var
            ~loc:core_type.ptyp_loc
            { loc = core_type.ptyp_loc; txt = sprintf "c%d" i }
        ) core_types
      |> ppat_tuple_opt ~loc
    in
    let mk_case = function
      | Rinherit _ -> raise_errorf ~loc "Inherited types not supported"
      | Rtag (name, _attributes, _bool, core_types) as row ->
        let f_name = { loc; txt = sprintf "_%s_of_tuple" name.txt } in
        let constr_name = match get_variant_name t row with
          | Some key -> key
          | None -> name.txt
        in
        (* If the inner type is a tuple, open it. *)
        let core_types = match core_types with
          | [ { ptyp_desc = Ptyp_tuple cts; _ } ] -> cts
          | cts -> cts
        in
        let f =
          let spec =
            List.map core_types ~f:(fun ct -> serialize_expr_of_type_descr t ~loc ct.ptyp_desc)
            |> slist_expr ~loc
          in
          [%expr [%e driver_func t ~loc "of_variant"] [%e estring ~loc constr_name]
              (Protocol_conv.Runtime.Variant_out.Tuple
                 Protocol_conv.Runtime.Tuple_out.([%e spec])
              )
          ]
        in
        let binding = value_binding ~loc ~pat:{ppat_desc = Ppat_var f_name; ppat_loc = loc; ppat_attributes=[]} ~expr:f in

        let lhs = ppat_variant ~loc name.txt (mk_pattern core_types) in
        let args =
          List.mapi ~f:(fun i _-> pexp_ident ~loc { loc; txt=Lident (sprintf "c%d" i) }) core_types
        in
        let rhs =
          pexp_apply ~loc (pexp_ident_string_loc f_name) (List.map ~f:(fun a -> (Nolabel, a)) args)
        in
        binding, case ~lhs ~guard:None ~rhs
    in
    let bindings, cases = List.map ~f:mk_case rows |> List.unzip in
    pexp_let ~loc Nonrecursive bindings @@ pexp_function ~loc cases
  | Ptyp_var core_type ->
    pexp_ident ~loc { loc; txt = Lident ( sprintf "__param_to_%s" core_type) }
  | Ptyp_arrow _ -> raise_errorf ~loc "Functions not supported"
  | Ptyp_variant _
  | Ptyp_any
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_alias _
  | Ptyp_package _
  | Ptyp_extension _ -> raise_errorf ~loc "Unsupported type descr"

let rec deserialize_variant t ~loc ~typ name core_types =
  let pexp_constr = match typ with
    | `Construct -> pexp_construct ~loc { loc; txt = Lident name }
    | `Variant -> pexp_variant ~loc name
  in
  match core_types with
  | [] -> pexp_constr None, [%expr Protocol_conv.Runtime.Tuple_in.Nil]
  | core_types ->
    let core_types = match core_types with
      | [ { ptyp_desc = Ptyp_tuple cts; _ } ] -> cts
      | cts -> cts
    in
    let constructor =
      let arg_names = List.mapi ~f:(fun i _ -> {loc; txt = Lident (sprintf "v%d" i)}) core_types in
      let body =
        pexp_tuple ~loc (List.map ~f:(pexp_ident ~loc) arg_names)
        |> Option.some
        |> pexp_constr
      in
      List.fold_right ~init:body ~f:(fun id expr ->
          pexp_fun ~loc Nolabel None (ppat_var ~loc (string_of_ident_loc id)) expr
        ) arg_names
    in
    let spec =
      List.map ~f:(fun ct -> deserialize_expr_of_type_descr t ~loc ct.ptyp_desc) core_types
      |> slist_expr ~loc
      |> fun e -> [%expr Protocol_conv.Runtime.Tuple_in.( [%e e] ) ]
    in
    constructor, spec

(** Deserialization expression for a given type *)
and deserialize_expr_of_type_descr t ~loc = function
  | Ptyp_constr ({ txt=Lident ident; loc }, [ct]) when is_meta_type ident ->
    let to_t = deserialize_expr_of_type_descr t ~loc ct.ptyp_desc in
    pexp_apply ~loc (driver_func t ~loc ("to_" ^ ident)) [Nolabel, to_t]

  | Ptyp_constr ({ txt=Lident ident; _ }, _) when is_meta_type ident ->
    raise_errorf ~loc "Unsupported type descr containing list of sub-types"

  | Ptyp_constr ({ txt=Lident s; loc }, _) when is_primitive_type s ->
    driver_func t ~loc ("to_" ^ s)

  | Ptyp_constr (ident, cts) ->
    (* Construct all arguments to of ... *)
    let args =
      List.map ~f:(fun { ptyp_desc; ptyp_loc; _ } ->
          deserialize_expr_of_type_descr t ~loc:ptyp_loc ptyp_desc) cts
      |> List.map ~f:(fun expr -> (Nolabel, expr))
    in
    let func = protocol_ident "of" t.driver ident in
    pexp_apply ~loc func args

  | Ptyp_tuple cts -> begin
      let constructor =
        let ids = List.mapi ~f:(fun i _ -> { loc; txt=Lident (sprintf "x%d" i) }) cts in
        let tuple =
          pexp_tuple ~loc (List.map ~f:(pexp_ident ~loc) ids)
        in
        List.fold_right ~init:tuple ~f:(fun id expr ->
            pexp_fun ~loc Nolabel None (ppat_var ~loc (string_of_ident_loc id)) expr
          ) ids
      in
      let slist =
        List.map ~f:(fun ct -> deserialize_expr_of_type_descr t ~loc ct.ptyp_desc) cts
      in
      [%expr
        let open !Protocol_conv.Runtime.Tuple_in in
        let _of_funcs = [%e slist_expr ~loc slist ] in
        let _constructor = [%e constructor] in
        [%e driver_func t ~loc "to_tuple"] _of_funcs _constructor
      ]
    end
  | Ptyp_poly _      ->
    raise_errorf ~loc "Polymorphic variants not supported"
  | Ptyp_variant (_rows, _closed, Some _) ->
    raise_errorf ~loc "Variant with some"

  | Ptyp_variant (rows, _closed, None) ->
    (* Variant deserialization *)
    test_row_mapping t rows;
    let mk_elem = function
      | Rinherit _ ->
        raise_errorf ~loc "Inherited types not supported"
      | Rtag (name, _attributes, _bool, core_types) as row ->
        let constructor, spec = deserialize_variant t ~loc ~typ:`Variant name.txt core_types in
        let name = match get_variant_name t row with
          | Some key -> key
          | None -> name.txt
        in
        [%expr
          ([%e estring ~loc name],
           Protocol_conv.Runtime.Variant_in.Tuple ([%e spec], [%e constructor]))
        ]
    in
    let arg_list =
      List.map ~f:mk_elem rows
      |> list_expr ~loc
    in
    [%expr
      [%e driver_func t ~loc "to_variant" ] [%e arg_list ]
    ]

  | Ptyp_var core_type -> pexp_ident ~loc { loc; txt = Lident ( sprintf "__param_of_%s" core_type) }

  | Ptyp_arrow _ -> raise_errorf ~loc "Functions not supported"
  | Ptyp_any
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_alias _
  | Ptyp_package _
  | Ptyp_extension _ -> raise_errorf ~loc "Unsupported type descr"

let test_label_mapping t labels =
  let base, mapped = List.partition_map ~f:(fun label ->
      match Attribute.get t.field_key label with
      | Some name when String.equal label.pld_name.txt name -> `Fst name
      | Some name -> `Snd (name, label.pld_attributes)
      | None -> `Fst label.pld_name.txt
    ) labels
  in
  let _: string list = List.fold_left ~init:base
      ~f:(fun acc -> function
          | (name, attribs) when List.mem ~equal:String.equal acc name ->
            let loc = location_of_attrib t "key" attribs in (* Should use the name of the attribute *)
            raise_errorf ~loc "Mapped label name in use: %s" name
          | (name, _) -> name :: acc
        ) mapped
  in
  ()

(** @returns function, pattern and arguments *)
let serialize_record t ~loc labels =
  test_label_mapping t labels;
  let spec =
    List.map ~f:(fun label ->
        let name =
          match Attribute.get t.field_key label with
          | None -> estring ~loc:label.pld_loc label.pld_name.txt
          | Some name -> estring ~loc:label.pld_loc name
        in
        let of_t = serialize_expr_of_type_descr t ~loc label.pld_type.ptyp_desc in
        let default =
          match Attribute.get t.field_default label with
          | None -> [% expr None]
          | Some expr -> [% expr Some [%e expr]]
        in
        [%expr ([%e name], [%e of_t], [%e default])]
      ) labels
    |> slist_expr ~loc
    |> fun spec -> [%expr Protocol_conv.Runtime.Record_out.( [%e spec] ) ]
  in
  spec,
  ppat_record ~loc
     (List.map ~f:(fun id ->
        { loc; txt=Lident id.txt }, ppat_var ~loc id)
        (List.map ~f:(fun ld -> ld.pld_name) labels)
     ) Closed,
  List.map ~f:(fun label -> Nolabel, pexp_ident ~loc { loc; txt=Lident label.pld_name.txt }) labels

let serialize_expr_of_tdecl t ~loc tdecl =
  match tdecl.ptype_kind with
  | Ptype_abstract -> begin
      match tdecl.ptype_manifest with
      | Some core_type ->
        serialize_expr_of_type_descr t ~loc core_type.ptyp_desc
      | None -> raise_errorf ~loc "Manifest is none"
    end
  | Ptype_variant constrs ->
    (* Serialize ADT *)
    test_constructor_mapping t constrs;
    let mk_pattern core_types =
      List.mapi ~f:(fun i (core_type:core_type) ->
          ppat_var
            ~loc:core_type.ptyp_loc
            { loc = core_type.ptyp_loc; txt = sprintf "c%d" i }
        ) core_types
      |> ppat_tuple_opt ~loc
    in
    let mk_case = function
      | { pcd_name; pcd_args = Pcstr_record labels; pcd_loc=loc; _ } as constr ->
        let spec, patt, args = serialize_record t ~loc labels in
        let f_name = { loc; txt = sprintf "_%s_of_record_" pcd_name.txt } in
        let constr_name = match get_constr_name t constr with
          | Some key -> key
          | None -> pcd_name.txt
        in
        let f = [%expr
          [%e driver_func t ~loc "of_variant"] [%e estring ~loc constr_name]
            (Protocol_conv.Runtime.Variant_out.Record [%e spec])
        ]
        in
        let lhs = ppat_construct
            ~loc
            (ident_of_string_loc pcd_name)
            (Some patt)
        in
        let rhs = pexp_apply ~loc (pexp_ident_string_loc f_name) args in
        let binding = value_binding ~loc ~pat:{ppat_desc = Ppat_var f_name; ppat_loc = loc; ppat_attributes=[]} ~expr:f in
        binding, case ~lhs ~guard:None ~rhs

      | { pcd_name; pcd_args = Pcstr_tuple core_types; pcd_loc=loc; _ } as constr ->
        let core_types = match core_types with
          | [ { ptyp_desc = Ptyp_tuple cts; _ } ] -> cts
          | cts -> cts
        in
        let f_name = { loc; txt = sprintf "_%s_of_tuple" pcd_name.txt } in
        let constr_name = match get_constr_name t constr with
          | Some key -> key
          | None -> pcd_name.txt
        in
        let f =
          let spec =
            List.map core_types ~f:(fun ct -> serialize_expr_of_type_descr t ~loc ct.ptyp_desc)
            |> slist_expr ~loc
          in
          [%expr [%e driver_func t ~loc "of_variant"] [%e estring ~loc constr_name]
              (Protocol_conv.Runtime.Variant_out.Tuple
                 Protocol_conv.Runtime.Tuple_out.([%e spec]))
          ]
        in
        let binding = value_binding ~loc ~pat:{ppat_desc = Ppat_var f_name; ppat_loc = loc; ppat_attributes=[]} ~expr:f in
        let lhs =
          ppat_construct
            ~loc
            (ident_of_string_loc pcd_name)
            (mk_pattern core_types)
        in
        let args =
          List.mapi ~f:(fun i _-> pexp_ident ~loc { loc; txt=Lident (sprintf "c%d" i) }) core_types
        in
        let rhs =
          pexp_apply ~loc (pexp_ident_string_loc f_name) (List.map ~f:(fun a -> (Nolabel, a)) args)
        in
        binding, case ~lhs ~guard:None ~rhs
    in
    let bindings, cases = List.map ~f:mk_case constrs |> List.unzip in
    pexp_let ~loc Nonrecursive bindings @@ pexp_function ~loc cases

  | Ptype_record labels ->
    let spec, patt, args = serialize_record t ~loc labels in
    [%expr
      let _of_record = [%e (driver_func t ~loc "of_record")] [%e spec] in
      fun [%p patt] ->
        [%e pexp_apply ~loc [%expr _of_record] args]
    ]
  | Ptype_open -> raise_errorf ~loc "Extensible variant types not supported"

let deserialize_record t ~loc ?map_result labels =
  test_label_mapping t labels;
  let field_ids = List.map ~f:(fun ld -> ld.pld_name) labels in

  let constructor =
    let record =
      pexp_record ~loc
        (List.map ~f:(fun id -> let id = { loc; txt=Lident id.txt } in id, pexp_ident ~loc id) field_ids)
        None
      |> fun r -> Option.value_map ~default:r ~f:(fun f -> f r) map_result
    in
    List.fold_right ~init:record ~f:(fun field expr ->
        pexp_fun ~loc Nolabel None (ppat_var ~loc field) expr
      ) field_ids
  in
  let spec =
    List.map ~f:(fun label ->
        let field_name =
          match Attribute.get t.field_key label with
          | None -> estring ~loc:label.pld_loc label.pld_name.txt
          | Some name -> estring ~loc:label.pld_loc name
        in
        let func = deserialize_expr_of_type_descr t ~loc label.pld_type.ptyp_desc in
        let default = match Attribute.get t.field_default label with
          | None -> [% expr None]
          | Some expr -> [% expr Some [%e expr]]
        in
        [%expr ([%e field_name], [%e func], [%e default])]
      ) labels
    |> slist_expr ~loc
  in
  (constructor,
  [%expr Protocol_conv.Runtime.Record_in.([%e spec])])

let deserialize_expr_of_tdecl t ~loc tdecl =
  match tdecl.ptype_kind with
  | Ptype_abstract -> begin
      match tdecl.ptype_manifest with
      | Some core_type ->
        deserialize_expr_of_type_descr t ~loc core_type.ptyp_desc
      | None -> raise_errorf ~loc "Manifest is none"
    end
  | Ptype_variant constrs ->
    (* Variant deserialization *)
    test_constructor_mapping t constrs;

    let mk_elem = function
      | { pcd_name; pcd_args = Pcstr_tuple core_types; pcd_loc=loc; _ } as constr ->
        let constructor, spec = deserialize_variant t ~loc ~typ:`Construct pcd_name.txt core_types in
        let name = match get_constr_name t constr with
          | Some key -> key
          | None -> pcd_name.txt
        in
        [%expr ([%e estring ~loc name ], Protocol_conv.Runtime.Variant_in.Tuple ([%e spec], [%e constructor])) ]
      | { pcd_name; pcd_args = Pcstr_record labels; pcd_loc=loc; _ } as constr ->
        let map_result x = pexp_construct ~loc { loc; txt = Lident pcd_name.txt } (Some x) in
        let constructor, spec = deserialize_record t ~loc ~map_result labels in
        let name = match get_constr_name t constr with
          | Some key -> key
          | None -> pcd_name.txt
        in
        [%expr ([%e estring ~loc name ], Protocol_conv.Runtime.Variant_in.Record ([%e spec], [%e constructor])) ]
    in
    let arg_list =
      List.map ~f:mk_elem constrs
      |> list_expr ~loc
    in
    [%expr
      [%e driver_func t ~loc "to_variant" ] [%e arg_list ]
    ]

  | Ptype_record labels ->
    let constructor, of_funcs = deserialize_record t ~loc labels in
    pexp_apply ~loc (driver_func t ~loc "to_record") [Nolabel, of_funcs; Nolabel, constructor]

  | Ptype_open -> raise_errorf ~loc "Extensible variant types not supported"

let serialize_function_name ~loc ~driver name =
  let prefix = match name.txt with
    | "t" -> ""
    | name -> name ^ "_"
  in
  sprintf "%sto_%s" prefix (module_name ~loc driver) |> Located.mk ~loc

let deserialize_function_name ~loc ~driver name =
  let prefix = match name.txt with
    | "t" -> ""
    | name -> name ^ "_"
  in
  sprintf "%sof_%s" prefix (module_name ~loc driver) |> Located.mk ~loc

let pstr_value_of_funcs ~loc rec_flag elements =
  List.map ~f:(fun (name, signature, expr) ->
      let pat =
        let p = (ppat_var ~loc name) in
        Option.value_map ~default:p ~f:(ppat_constraint ~loc p) signature
      in
      Ast_helper.Vb.mk ~loc pat expr
    ) elements
  |> pstr_value ~loc rec_flag

let name_of_core_type ~prefix = function
  | { ptyp_desc = Ptyp_var var; ptyp_loc; _ } ->
    { loc = ptyp_loc; txt = sprintf "__param_%s_%s" prefix var }
  | { ptyp_desc = Ptyp_any; ptyp_loc; _ } ->
    raise_errorf ~loc:ptyp_loc "Generalized algebraic datatypes not supported"
  | { ptyp_desc = Ptyp_arrow (_, _, _); _} -> failwith "Ptyp_arrow "
  | { ptyp_desc = Ptyp_tuple _; _} -> failwith "Ptyp_tuple "
  | { ptyp_desc = Ptyp_constr (_, _); _} -> failwith "Ptyp_constr "
  | { ptyp_desc = Ptyp_object (_, _); _} -> failwith "Ptyp_object "
  | { ptyp_desc = Ptyp_class (_, _); _} -> failwith "Ptyp_class "
  | { ptyp_desc = Ptyp_alias (_, _); _} -> failwith "Ptyp_alias "
  | { ptyp_desc = Ptyp_variant (_, _, _); _} -> failwith "Ptyp_variant "
  | { ptyp_desc = Ptyp_poly (_, _); _} -> failwith "Ptyp_poly "
  | { ptyp_desc = Ptyp_package _; _} -> failwith "Ptyp_package "
  | { ptyp_desc = Ptyp_extension _; _} -> failwith "Ptyp_extension "

(** Test if a type references itself, in which case we cannot do eager evaluation.
    We could create a reference to the potentially evaluated function instead:
    Its not as fast, but still much faster!

    let rec of_driver =
      let f = ref None in
      fun t -> match !f with
               | Some f -> f
               | None ->
                 f := ...;
                 !f
*)

let rec is_recursive_ct types = function
  | { ptyp_desc = Ptyp_var var; _ } ->
    List.mem types var ~equal:String.equal
  | { ptyp_desc = Ptyp_any; _ } -> false
  | { ptyp_desc = Ptyp_arrow _; _} -> false
  | { ptyp_desc = Ptyp_tuple cts; _} -> List.exists ~f:(is_recursive_ct types) cts
  | { ptyp_desc = Ptyp_constr (l, cts); _} ->
    List.mem types (string_of_ident_loc l).txt ~equal:String.equal ||
    List.exists ~f:(is_recursive_ct types) cts
  | { ptyp_desc = Ptyp_object _; _} -> false
  | { ptyp_desc = Ptyp_class _; _} -> false
  | { ptyp_desc = Ptyp_alias (c, _); _} -> is_recursive_ct types c
  | { ptyp_desc = Ptyp_variant (rows, _, _); _} ->
    List.exists ~f:(function
        | Rtag (_, _, _, cts) -> List.exists ~f:(is_recursive_ct types) cts
        | Rinherit _ -> false
      ) rows
  | { ptyp_desc = Ptyp_poly (_, ct); _} -> is_recursive_ct types ct
  | { ptyp_desc = Ptyp_package _; _} -> false
  | { ptyp_desc = Ptyp_extension _; _} -> false

let is_recursive types = function
  | Ptype_abstract -> false
  | Ptype_variant (cstr_decls) ->
    List.exists ~f:(function
        | { pcd_args = Pcstr_tuple cts; _ } ->
          List.exists ~f:(is_recursive_ct types) cts
        | { pcd_args = Pcstr_record ldecls; _ } ->
          List.exists ~f:(fun { pld_type = ct; _} -> is_recursive_ct types ct) ldecls
      ) cstr_decls
  | Ptype_record ldecls ->
    List.exists ~f:(fun { pld_type = ct; _} -> is_recursive_ct types ct) ldecls
  | Ptype_open -> false

let is_recursive tydecls = function
  | Nonrecursive -> false
  | Recursive ->
    let names = List.map ~f:(fun { ptype_name = { txt=name; _}; _ } -> name) tydecls in
    List.exists ~f:(fun { ptype_kind; ptype_params = ctvl; ptype_manifest; _} ->
        is_recursive names ptype_kind ||
        List.exists ~f:(fun (ct, _var) -> is_recursive_ct names ct) ctvl ||
        Option.value_map ptype_manifest ~default:false ~f:(is_recursive_ct names)
      ) tydecls

(* Add type parameters *)
let mk_typ ~loc tydecl =
  let params =
    List.filter_map
      ~f:(function ({ ptyp_desc = Ptyp_var s; _ }, _variance) -> Some s
                 | _ -> None) tydecl.ptype_params
    |> List.map ~f:(fun param -> ptyp_var ~loc param)
  in
  ptyp_constr ~loc { loc; txt = Lident tydecl.ptype_name.txt } params

let type_of_to_func ~loc driver tydecl =
  [%type: [%t mk_typ ~loc tydecl] -> [%t ptyp_constr ~loc { loc; txt = Ldot (driver, "t")} [] ] ]

let serialization_signature ~loc driver tdecl =
  let type_of = type_of_to_func ~loc driver tdecl in
  let params =
    List.filter_map
      ~f:(function ({ ptyp_desc = Ptyp_var s; _ }, _variance) -> Some s
                 | _ -> None) tdecl.ptype_params
  in
  List.fold_right params ~init:type_of ~f:(fun name acc ->
      let typ =
        ptyp_arrow ~loc Nolabel
          (ptyp_var ~loc name)
          (ptyp_constr ~loc {loc; txt = Ldot (driver, "t")} [])
      in
      ptyp_arrow ~loc Nolabel typ acc
    )
  |> ptyp_poly ~loc (List.map ~f:(fun txt -> { loc; txt }) params)

let make_recursive ~loc (e : expression) = function
  | false -> e
  | true ->
    [%expr (let f = ref None in
            (fun t -> match !f with
               | None ->
                 let f' = [%e e] in f := Some f'; f' t
               | Some f -> f t
            ))
    ]


let to_protocol_str_type_decls t rec_flag ~loc tydecls =
  let is_recursive = is_recursive tydecls rec_flag in
  List.map
    ~f:(fun tdecl ->
        let to_p = serialize_function_name ~loc ~driver:t.driver tdecl.ptype_name in
        let expr = make_recursive ~loc (serialize_expr_of_tdecl t ~loc tdecl) is_recursive in
        let expr_param =
          List.fold_right ~init:expr ~f:(fun (ct, _variance) expr ->
              let patt = Ast_helper.Pat.var ~loc (name_of_core_type ~prefix:"to" ct) in
              [%expr fun [%p patt] -> [%e expr] ]
            ) tdecl.ptype_params
        in
        let signature = serialization_signature t.driver ~loc tdecl in
        (to_p, Some signature, expr_param)
      ) tydecls
    |> pstr_value_of_funcs ~loc (if is_recursive then rec_flag else Nonrecursive)
    |> fun x -> [ x ]

let of_protocol_str_type_decls t rec_flag ~loc tydecls =
  let is_recursive = is_recursive tydecls rec_flag in
  List.map
    ~f:(fun tdecl ->
        let of_p = deserialize_function_name ~loc ~driver:t.driver tdecl.ptype_name in
        let expr = make_recursive ~loc (deserialize_expr_of_tdecl t ~loc tdecl) is_recursive in
        let expr_param =
          List.fold_right ~init:expr ~f:(fun (ct, _variance) expr ->
              let patt = Ast_helper.Pat.var ~loc (name_of_core_type ~prefix:"of" ct) in
              [%expr fun [%p patt] -> [%e expr] ])
            tdecl.ptype_params
        in
        let signature = None in
        (of_p, signature, expr_param)
      ) tydecls
  |> pstr_value_of_funcs ~loc (if is_recursive then rec_flag else Nonrecursive)
  |> fun x -> [x]

let protocol_str_type_decls t rec_flag ~loc tydecls =
  to_protocol_str_type_decls t rec_flag ~loc tydecls @
  of_protocol_str_type_decls t rec_flag ~loc tydecls

let to_protocol_sig_type_decls ~loc ~path:_ (_rec_flag, tydecls) (driver:module_expr option) =
  let driver = ident_of_module ~loc driver in
  List.concat_map ~f:(fun tydecl ->
      let signature = serialization_signature ~loc driver tydecl in
      (*
      let to_type = type_of_to_func ~loc driver tydecl in
      *)
      let to_p = serialize_function_name ~loc ~driver tydecl.ptype_name in
      psig_value ~loc (value_description ~loc ~name:to_p ~type_:signature ~prim:[]) :: []
    ) tydecls

let of_protocol_sig_type_decls ~loc ~path:_ (_rec_flag, tydecls) (driver:module_expr option) =
  let driver = ident_of_module ~loc driver in
  List.concat_map ~f:(fun tydecl ->
      let of_p = deserialize_function_name ~loc ~driver tydecl.ptype_name  in
      let of_type = [%type:
        [%t ptyp_constr ~loc { loc; txt = Ldot (driver, "t")} [] ]
        -> [%t mk_typ ~loc tydecl]] in
      psig_value ~loc (value_description ~loc ~name:of_p ~type_:of_type ~prim:[]) :: []
    ) tydecls

let protocol_sig_type_decls ~loc ~path (rec_flag, tydecls) (driver:module_expr option) =
  to_protocol_sig_type_decls ~loc ~path (rec_flag, tydecls) driver @
  of_protocol_sig_type_decls ~loc ~path (rec_flag, tydecls) driver

let mk_str_type_decl =
  (* Cache to avoid creating the same attributes twice. *)
  let attrib_table = Hashtbl.Poly.create () in
  fun f ~loc ~path:_ (recflag, tydecls) driver ->
    (* Create T and pass on to f *)
    let driver = ident_of_module ~loc driver in
    let attrib_name name = sprintf "%s.%s" (module_name driver) name in
    let field_key, constr_key, constr_name, variant_key, variant_name, field_default =
      let create () =
        let open Attribute in
        declare (attrib_name "key")
          Context.label_declaration
          Ast_pattern.(single_expr_payload (estring __)) (fun x -> x),
        declare (attrib_name "key") (* Deprecated *)
          Context.constructor_declaration
          Ast_pattern.(single_expr_payload (estring __)) (fun x -> x),
        declare (attrib_name "name")
          Context.constructor_declaration
          Ast_pattern.(single_expr_payload (estring __)) (fun x -> x),
        declare (attrib_name "key") (* Deprecated *)
          Context.rtag
          Ast_pattern.(single_expr_payload (estring __)) (fun x -> x),
        declare (attrib_name "name")
          Context.rtag
          Ast_pattern.(single_expr_payload (estring __)) (fun x -> x),
        declare (attrib_name "default")
          Context.label_declaration
          Ast_pattern.(single_expr_payload (__)) (fun x -> x)
      in
      Hashtbl.find_or_add attrib_table driver ~default:create
    in
    let t = {
      driver;
      field_key;
      constr_key;
      constr_name;
      variant_key;
      variant_name;
      field_default;
    } in
    f t recflag ~loc tydecls

let () =
  let driver = Ppxlib.Deriving.Args.(arg "driver" (pexp_pack __)) in
  Deriving.add "protocol"
    ~str_type_decl:(Deriving.Generator.make
                      Deriving.Args.(empty +> driver)
                      (mk_str_type_decl protocol_str_type_decls))
    ~sig_type_decl:(Deriving.Generator.make Deriving.Args.(empty +> driver) protocol_sig_type_decls)
  |> Ppxlib.Deriving.ignore;

  Deriving.add "of_protocol"
    ~str_type_decl:(Deriving.Generator.make
                      Deriving.Args.(empty +> driver)
                      (mk_str_type_decl of_protocol_str_type_decls))
    ~sig_type_decl:(Deriving.Generator.make Deriving.Args.(empty +> driver) of_protocol_sig_type_decls)
  |> Deriving.ignore;

  Deriving.add "to_protocol"
    ~str_type_decl:(Deriving.Generator.make
                      Deriving.Args.(empty +> driver)
                      (mk_str_type_decl to_protocol_str_type_decls))
    ~sig_type_decl:(Deriving.Generator.make Deriving.Args.(empty +> driver) to_protocol_sig_type_decls)
  |> Deriving.ignore;
