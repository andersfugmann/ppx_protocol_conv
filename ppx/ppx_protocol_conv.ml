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

let string_of_ident_loc { loc; txt } =
  let rec inner = function
    | Lident s -> s
    | Ldot (i, s) -> inner i ^ "." ^ s
    | Lapply _  -> raise_errorf ~loc "lapply???"
  in
  { loc; txt=inner txt }

let rec string_of_ident = function
  | Lident s -> s
  | Ldot (i, s) -> string_of_ident i ^ "." ^ s
  | Lapply _  -> raise_errorf "lapply???"

let ident_of_string_loc { loc; txt} = { loc; txt=Lident txt }

let driver_func t ~loc name =
  let func = pexp_ident ~loc { loc; txt = Ldot (t.driver, name) } in
  [%expr (fun t -> [%e func] t)]

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
  | "string" | "int" | "int32" | "int64" | "float" | "bool" | "char" | "unit" -> true
  | _ -> false

(** Test if the type is a type modifier *)
let is_meta_type = function
  | "option" | "array" | "list" | "lazy_t" | "ref" -> true
  | _ -> false

let type_param_name ~prefix var =
  sprintf "__param_%s_'%s" prefix var

let module_name ?loc = function
  | Lident s -> String.uncapitalize s
  | Ldot (_, s) -> String.uncapitalize s
  | Lapply _ -> raise_errorf ?loc "lapply???"

let default_case ~loc =
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
      let to_ps = List.map ~f:
          (fun ct -> serialize_expr_of_type_descr t ~loc ct.ptyp_desc ) cts
      in
      let ids = List.mapi ~f:(fun i _ -> { loc; txt=Lident (sprintf "x%d" i) }) cts in

      let _arg_list =
        List.mapi ~f:(fun i ct ->
            let to_ps = serialize_expr_of_type_descr t ~loc ct.ptyp_desc in
            [%expr (
              [%e estring ~loc (sprintf "t%d" i)],
              [%e pexp_ident ~loc { loc; txt=Lident (sprintf "x%d" i) }],
              [%e to_ps],
              None)
            ]
          ) cts
        |> list_expr ~loc
      in
      let arg_list =
        List.map2_exn
          ~f:(fun id func -> pexp_apply ~loc func [Nolabel, pexp_ident ~loc id])
          ids to_ps
        |> List.mapi ~f:(fun i expr -> [%expr ([%e estring ~loc (sprintf "t%d" i)], [%e expr])])
        |> list_expr ~loc
      in
      pexp_fun ~loc Nolabel None
        (ppat_tuple ~loc (List.map ~f:(fun id ->ppat_var ~loc (string_of_ident_loc id)) ids))
        (pexp_apply ~loc (driver_func t ~loc "of_tuple") [Nolabel, arg_list])
    end
  | Ptyp_poly _      -> raise_errorf ~loc "Polymorphic variants not supported"
  | Ptyp_variant (rows, _closed, None) ->
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
      | Rinherit _ ->
        raise_errorf ~loc "Inherited types not supported"
      | Rtag (name, _attributes, _bool, core_types) as row ->
        let lhs =
          ppat_variant ~loc name.txt (mk_pattern core_types)
        in
        let args =
          List.mapi ~f:(
            fun i core_type ->
              pexp_apply ~loc
                (serialize_expr_of_type_descr t ~loc core_type.ptyp_desc)
                [Nolabel, pexp_ident ~loc { loc; txt=Lident (sprintf "c%d" i) }]
          ) core_types
        in
        let rhs =
          let constr_name = match get_variant_name t row with
            | Some key -> key
            | None -> name.txt
          in
          [%expr ( [%e estring ~loc constr_name ],
                   [%e args |> list_expr ~loc] )]
        in
        case ~lhs ~guard:None ~rhs
    in
    [%expr
      [%e driver_func t ~loc "of_variant" ]
        [%e pexp_function ~loc (List.map ~f:mk_case rows) ]
    ]
  | Ptyp_var core_type -> pexp_ident ~loc { loc; txt = Lident ( type_param_name ~prefix:"to" core_type) }
  | Ptyp_arrow _ -> raise_errorf ~loc "Functions not supported"
  | Ptyp_variant _
  | Ptyp_any
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_alias _
  | Ptyp_package _
  | Ptyp_extension _ -> raise_errorf ~loc "Unsupported type descr"

(** Deserialization expression for a given type *)
let rec deserialize_expr_of_type_descr t ~loc = function
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
      let to_ts = List.map ~f:
          (fun ct -> deserialize_expr_of_type_descr t ~loc ct.ptyp_desc,
                     [% expr None]
          ) cts
      in
      let ids = List.mapi ~f:(fun i _ -> { loc; txt=Lident (sprintf "x%d" i) }) cts in
      let constructor =
        let tuple =
          pexp_tuple ~loc (List.map ~f:(pexp_ident ~loc) ids)
        in
        List.fold_right ~init:tuple ~f:(fun id expr ->
            pexp_fun ~loc Nolabel None (ppat_var ~loc (string_of_ident_loc id)) expr
          ) ids
      in
      let slist =
        List.mapi ~f:(fun i (v, d) ->
            [%expr (
              [%e estring ~loc (sprintf "t%d" i)],
              [%e v],
              [%e d])]
          ) to_ts
      in
      [%expr
        let open !Protocol_conv.Runtime.Record_in in
        let of_funcs = [%e slist_expr ~loc slist ] in
        let constructor = [%e constructor] in
        [%e driver_func t ~loc "to_tuple"] of_funcs constructor
      ]
    end
  | Ptyp_poly _      ->
    raise_errorf ~loc "Polymorphic variants not supported"
  | Ptyp_variant (_rows, _closed, Some _) ->
    raise_errorf ~loc "Variant with some"

  | Ptyp_variant (rows, _closed, None) ->
    test_row_mapping t rows;
    let mk_case = function
      | Rinherit _ ->
        raise_errorf ~loc "Inherited types not supported"
      | Rtag (name, _attributes, _bool, core_types) as row ->
        (* val: to_variant: ((string * t list) -> 'a) -> t -> 'a *)
        let lhs =
          let constr_name = match get_variant_name t row with
            | Some key -> key
            | None -> name.txt
          in
          let pcstr s pat = ppat_construct ~loc { loc; txt=Lident s } pat in
          core_types
          |> List.rev_mapi ~f:(fun i _ -> sprintf "c%d" i)
          |> List.map ~f:(fun s -> { loc; txt=s })
          |> List.fold_left ~init:(pcstr "[]" None)
            ~f:(fun acc var ->
                pcstr "::" (Some (ppat_tuple ~loc
                                    [ppat_var ~loc var; acc]))
              )
          |> fun p -> ppat_tuple ~loc [ ppat_constant ~loc (Pconst_string (constr_name, None)); p ]
        in
        let rhs =
          (* A function c1 c2 c3 -> A (to_t c1, to_t c2, to_t c3) *)
          let of_p var core_type =
            let e = deserialize_expr_of_type_descr t ~loc core_type in
            pexp_apply ~loc e [Nolabel, pexp_ident ~loc { loc; txt=Lident var }]
          in

          let args = match core_types with
            | [] -> None
            | cts -> List.mapi ~f:(fun i ct -> of_p (sprintf "c%d" i) ct.ptyp_desc ) cts
                    |> pexp_tuple ~loc
                    |> Option.some
          in
          pexp_variant ~loc name.txt args
        in
        case ~lhs ~guard:None ~rhs
    in
    [%expr
      [%e driver_func t ~loc "to_variant" ]
        [%e pexp_function ~loc (List.map ~f:mk_case rows |> fun ps -> ps @ [default_case ~loc]) ]
    ]


  | Ptyp_var core_type -> pexp_ident ~loc { loc; txt = Lident ( type_param_name ~prefix:"of" core_type) }
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


let serialize_record t ~loc labels =
  test_label_mapping t labels;
  let arg_list =
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
        [%expr ([%e name],
               [%e pexp_ident ~loc { loc; txt=Lident label.pld_name.txt } ],
               [%e of_t],
               [%e default])
        ]
      ) labels
    |> slist_expr ~loc
    |> fun l -> [%expr let open !Protocol_conv.Runtime.Record_in in [%e l]]
  in
  ppat_record ~loc
     (List.map ~f:(fun id ->
        { loc; txt=Lident id.txt }, ppat_var ~loc id)
        (List.map ~f:(fun ld -> ld.pld_name) labels)
     ) Closed,
  pexp_apply ~loc (driver_func t ~loc "of_record") [Nolabel, arg_list]


let serialize_expr_of_tdecl t ~loc tdecl =
  match tdecl.ptype_kind with
  | Ptype_abstract -> begin
      match tdecl.ptype_manifest with
      | Some core_type ->
        serialize_expr_of_type_descr t ~loc core_type.ptyp_desc
      | None -> raise_errorf ~loc "Manifest is none"
    end
  | Ptype_variant constrs ->
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
        let pattern, body = serialize_record t ~loc labels in
        let lhs = ppat_construct
            ~loc
            (ident_of_string_loc pcd_name)
            (Some pattern)
        in
        let rhs =
          let constr_name = match get_constr_name t constr with
            | Some key -> key
            | None -> pcd_name.txt
          in
          [%expr ( [%e estring ~loc constr_name ], [ [%e body] ]) ]
        in
        case ~lhs ~guard:None ~rhs

      | { pcd_name; pcd_args = Pcstr_tuple core_types; pcd_loc=loc; _ } as constr ->
        let lhs =
          ppat_construct
            ~loc
            (ident_of_string_loc pcd_name)
            (mk_pattern core_types)
        in
        (* of_variant "A" [ of_int c1; of_list c2; ... ] *)
        let args =
          List.mapi ~f:(
            fun i core_type ->
              pexp_apply ~loc
              (serialize_expr_of_type_descr t ~loc core_type.ptyp_desc)
              [Nolabel, pexp_ident ~loc { loc; txt=Lident (sprintf "c%d" i) }]
          ) core_types
        in
        let rhs =
          let constr_name = match get_constr_name t constr with
            | Some key -> key
            | None -> pcd_name.txt
          in
          [%expr ( [%e estring ~loc constr_name ],
                   [%e args |> list_expr ~loc] )]
        in
        case ~lhs ~guard:None ~rhs
    in
    [%expr
      [%e driver_func t ~loc "of_variant" ]
        [%e pexp_function ~loc (List.map ~f:mk_case constrs) ]
    ]
  | Ptype_record labels ->
    let pattern, body = serialize_record t ~loc labels in
    pexp_fun ~loc Nolabel None pattern body
  | Ptype_open -> raise_errorf ~loc "Extensible variant types not supported"


let deserialize_record t ~loc labels =
  test_label_mapping t labels;
  let field_ids = List.map ~f:(fun ld -> ld.pld_name) labels in

  let constructor f =
    let record =
      pexp_record ~loc
        (List.map ~f:(fun id -> let id = { loc; txt=Lident id.txt } in id, pexp_ident ~loc id) field_ids)
        None
      |> f
    in
    List.fold_right ~init:record ~f:(fun field expr ->
        pexp_fun ~loc Nolabel None (ppat_var ~loc field) expr
      ) field_ids
  in
  let list_items = List.map ~f:(fun label ->
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
  in
  let of_funcs = slist_expr ~loc list_items in
  (constructor, [%expr Protocol_conv.Runtime.Record_in.([%e of_funcs])])

let deserialize_expr_of_tdecl t ~loc tdecl =
  match tdecl.ptype_kind with
  | Ptype_abstract -> begin
      match tdecl.ptype_manifest with
      | Some core_type ->
        deserialize_expr_of_type_descr t ~loc core_type.ptyp_desc
      | None -> raise_errorf ~loc "Manifest is none"
    end
  | Ptype_variant constrs ->
    test_constructor_mapping t constrs;
    let mk_case = function
      | { pcd_name; pcd_args = Pcstr_record labels; pcd_loc=loc; _ } as constr ->
        let lhs =
          let constr_name = match get_constr_name t constr with
            | Some key -> key
            | None -> pcd_name.txt
          in
          let name = ppat_constant ~loc (Pconst_string (constr_name, None)) in
          [%pat? ([%p name], [x])]
        in
        let rhs =
          let constructor, of_funcs = deserialize_record t ~loc labels in
          let wrap expr = pexp_construct (ident_of_string_loc pcd_name) ~loc (Some expr) in
          [%expr
            let of_funcs = [%e of_funcs ] in
            let constructor = [%e constructor wrap ] in
            [%e driver_func t ~loc "to_record"] of_funcs constructor x
          ]
        in
        case ~lhs ~guard:None ~rhs

      | { pcd_name; pcd_args = Pcstr_tuple core_types; pcd_loc=loc; _ } as constr ->
        let lhs =
          let constr_name = match get_constr_name t constr with
            | Some key -> key
            | None -> pcd_name.txt
          in
          let pcstr s pat = ppat_construct ~loc { loc; txt=Lident s } pat in
          core_types
          |> List.rev_mapi ~f:(fun i _ -> sprintf "c%d" i)
          |> List.map ~f:(fun s -> { loc; txt=s })
          |> List.fold_left ~init:(pcstr "[]" None)
            ~f:(fun acc var ->
                pcstr "::" (Some (ppat_tuple ~loc

                                    [ppat_var ~loc var; acc]))
              )
          |> fun p -> ppat_tuple ~loc [ ppat_constant ~loc (Pconst_string (constr_name, None)); p ]
        in
        let rhs =
          (* A function c1 c2 c3 -> A (to_t c1, to_t c2, to_t c3) *)
          let of_p var core_type =
            let e = deserialize_expr_of_type_descr t ~loc core_type in
            pexp_apply ~loc e [Nolabel, pexp_ident ~loc { loc; txt=Lident var }]
          in

          let args = match core_types with
            | [] -> None
            | cts -> List.mapi ~f:(fun i ct -> of_p (sprintf "c%d" i) ct.ptyp_desc ) cts
                    |> pexp_tuple ~loc
                    |> Option.some
          in
          pexp_construct (ident_of_string_loc pcd_name) ~loc args
        in
        case ~lhs ~guard:None ~rhs
    in
    [%expr
      [%e driver_func t ~loc "to_variant" ]
        [%e pexp_function ~loc (List.map ~f:mk_case constrs |> fun ps -> ps @ [default_case ~loc]) ]
    ]
  | Ptype_record labels ->
    let constructor, of_funcs = deserialize_record t ~loc labels in
    [%expr
      let of_funcs = [%e of_funcs ] in
      let constructor = [%e constructor Fn.id ] in
      [%e driver_func t ~loc "to_record"] of_funcs constructor
    ]

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

(* This needs the disabling of warnings *)
let pstr_value_of_funcs ~loc rec_flag elements =
  List.map ~f:(fun (name, expr) ->
      let pat = ppat_var ~loc name in
      Ast_helper.Vb.mk
        ~attrs:[{ loc; txt="ocaml.warning" }, PStr [%str "-39"]]
        ~loc pat expr
    ) elements
  |> pstr_value ~loc rec_flag

let name_of_core_type ~prefix = function
  | { ptyp_desc = Ptyp_var var; ptyp_loc; _ } ->
    { loc = ptyp_loc; txt = type_param_name ~prefix var }
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

let to_protocol_str_type_decls t rec_flag ~loc tydecls =
  pstr_value_of_funcs ~loc rec_flag
    ( List.map
        ~f:(fun tdecl ->
            let name = tdecl.ptype_name in
            let to_p = serialize_function_name ~loc ~driver:t.driver name in
            let expr =
              [%expr fun t -> [%e serialize_expr_of_tdecl t ~loc tdecl] t]
            in
            let expr_param =
              List.fold_right ~init:expr ~f:(fun (ct, _variance) expr ->
                  let patt = Ast_helper.Pat.var ~loc (name_of_core_type ~prefix:"to" ct) in
                  [%expr fun [%p patt] -> [%e expr] ])
                tdecl.ptype_params
            in

            (to_p, expr_param)
          ) tydecls
    )
  :: []

let of_protocol_str_type_decls t rec_flag ~loc tydecls =
  pstr_value_of_funcs ~loc rec_flag
    ( List.map
        ~f:(fun tdecl ->
            let name = tdecl.ptype_name in
            let of_p = deserialize_function_name ~loc ~driver:t.driver name in

            let expr =
              [%expr fun t -> [%e deserialize_expr_of_tdecl t ~loc tdecl] t]
            in
            let expr_param =
              List.fold_right ~init:expr ~f:(fun (ct, _variance) expr ->
                let patt = Ast_helper.Pat.var ~loc (name_of_core_type ~prefix:"of" ct) in
                [%expr fun [%p patt] -> [%e expr] ])
                tdecl.ptype_params
            in
            (of_p, expr_param)
          ) tydecls
    )
  :: []

let protocol_str_type_decls t rec_flag ~loc tydecls =
  to_protocol_str_type_decls t rec_flag ~loc tydecls @
  of_protocol_str_type_decls t rec_flag ~loc tydecls

let mk_typ ~loc name =
  ptyp_constr ~loc (Located.mk ~loc (Longident.parse name)) []

let to_protocol_sig_type_decls ~loc ~path:_ (_rec_flag, tydecls) (driver:module_expr option) =
  let driver = ident_of_module ~loc driver in
  List.concat_map ~f:(fun tydecl ->
      let name = tydecl.ptype_name in
      let result = string_of_ident (Ldot (driver, "t")) in
      let to_p = serialize_function_name ~loc ~driver name  in
      let to_type = [%type: [%t mk_typ ~loc name.txt] -> [%t mk_typ ~loc result]] in
      psig_value ~loc (value_description ~loc ~name:to_p ~type_:to_type ~prim:[]) :: []
    ) tydecls

let of_protocol_sig_type_decls ~loc ~path:_ (_rec_flag, tydecls) (driver:module_expr option) =
  let driver = ident_of_module ~loc driver in
  List.concat_map ~f:(fun tydecl ->
      let name = tydecl.ptype_name in
      let result = string_of_ident (Ldot (driver, "t")) in
      let of_p = deserialize_function_name ~loc ~driver name  in
      let of_type = [%type: [%t mk_typ ~loc result] -> [%t mk_typ ~loc name.txt]] in
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
