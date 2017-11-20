open Ppx_type_conv.Std
open Ppx_core
open Ast_builder.Default

open !Printf

(* In variants, dont encode as tuple.... *)
let raise_errorf ?loc fmt = Location.raise_errorf ?loc ("ppx_protocol_conv: " ^^ fmt)

let expr_of_ident ?(prefix="") ?(suffix="") { loc; txt } =
  let txt = match txt with
    | Lident s -> Lident (prefix ^ s ^ suffix)
    | Ldot (l, s) -> Ldot (l, prefix ^ s ^ suffix)
    | Lapply _  -> raise_errorf ~loc "lapply???"
  in
  pexp_ident ~loc { loc; txt }

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

let driver_func ~driver ~flags ~loc name =
  let func = pexp_ident ~loc { loc; txt = Ldot (driver, name) } in
  match flags with
  | None ->[%expr (fun t -> [%e func] t)]
  | Some flag -> [%expr [%e func] ~flags:[%e flag] ]

(** Concatinate the list of expressions into a single expression using list concatination *)
let list_expr ~loc l =
  List.fold_left ~init:[%expr []] ~f:(fun tl hd -> [%expr [%e hd] :: [%e tl]]) (List.rev l)

let spec_expr ~loc l =
  List.fold_left ~init:[%expr Nil] ~f:(fun tl (e1, e2) -> [%expr ([%e e1], [%e e2]) ^:: [%e tl]]) (List.rev l)

let ident_of_module ~loc = function
  | Some { pmod_desc = Pmod_ident { txt; _ }; _ } -> txt
  | Some _ -> raise_errorf ~loc "must be a module identifier"
  | None -> raise_errorf ~loc "~driver argument missing"

let is_primitive_type = function
  | "string" | "int" | "int32" | "int64" | "float" | "bool" | "unit" -> true
  | _ -> false

let is_meta_type = function
  | "option" | "array" | "list" -> true
  | _ -> false

let module_name ?loc = function
  | Lident s -> String.uncapitalize s
  | Ldot (_, s) -> String.uncapitalize s
  | Lapply _ -> raise_errorf ?loc "lapply???"

(* Can only be declared once. Declare or get???? *)
let key_attrib =
  let table = Hashtbl.Poly.create () in
  let create key_name =
    let open Attribute in
    declare key_name Context.label_declaration Ast_pattern.(single_expr_payload (estring __)) (fun x -> x)
  in
  fun ~driver ->
    let name = sprintf "%s.key" (module_name driver) in
    Hashtbl.find_or_add table name ~default:(fun () -> create name)

let rec serialize_expr_of_type_descr ~loc ~(driver:longident) ~flags = function
  | Ptyp_constr ({ txt=Lident ident; loc }, [ct]) when is_meta_type ident ->
    let to_p = serialize_expr_of_type_descr ~loc ~driver ~flags ct.ptyp_desc in
    pexp_apply ~loc (driver_func ~loc ~driver ~flags ("of_" ^ ident)) [Nolabel, to_p]

  | Ptyp_constr ({ txt=Lident ident; _ }, _) when is_meta_type ident ->
    raise_errorf ~loc "Unsupported type descr containing list of sub-types"

  | Ptyp_constr ({ txt=Lident s; loc }, _) when is_primitive_type s ->
    driver_func ~loc ~driver ~flags ("of_" ^ s)

  | Ptyp_constr (ident, _) ->
    let driver = module_name ~loc driver in
    expr_of_ident ~suffix:("_to_" ^ driver) ident

  | Ptyp_tuple cts -> begin
      let to_ps = List.map ~f:
          (fun ct -> serialize_expr_of_type_descr ~loc ~driver ~flags ct.ptyp_desc ) cts
      in
      let ids = List.mapi ~f:(fun i _ -> { loc; txt=Lident (sprintf "x%d" i) }) cts in

      let arg_list =
        List.map2_exn
          ~f:(fun id func -> pexp_apply ~loc func [Nolabel, pexp_ident ~loc id])
          ids to_ps
        |> List.mapi ~f:(fun i expr -> [%expr ([%e estring ~loc (sprintf "t%d" i)], [%e expr])])
        |> list_expr ~loc
      in

      pexp_fun ~loc Nolabel None
        (ppat_tuple ~loc (List.map ~f:(fun id ->ppat_var ~loc (string_of_ident_loc id)) ids))
        (pexp_apply ~loc (driver_func ~loc ~driver ~flags "of_tuple") [Nolabel, arg_list])
    end
  | Ptyp_poly _ -> raise_errorf ~loc "Polymorphic variants not supported"
  | Ptyp_variant _ -> raise_errorf ~loc "Variant type descr not supported"
  | Ptyp_any
  | Ptyp_var _
  | Ptyp_arrow _
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_alias _
  | Ptyp_package _
  | Ptyp_extension _ -> raise_errorf ~loc "Unsupported type descr"



let rec deserialize_expr_of_type_descr ~loc ~(driver:longident) ~flags = function
  | Ptyp_constr ({ txt=Lident ident; loc }, [ct]) when is_meta_type ident ->
    let to_t = deserialize_expr_of_type_descr ~loc ~driver ~flags ct.ptyp_desc in
    pexp_apply ~loc (driver_func ~loc ~driver ~flags ("to_" ^ ident)) [Nolabel, to_t]

  | Ptyp_constr ({ txt=Lident ident; _ }, _) when is_meta_type ident ->
    raise_errorf ~loc "Unsupported type descr containing list of sub-types"

  | Ptyp_constr ({ txt=Lident s; loc }, _) when is_primitive_type s ->
    driver_func ~loc ~driver ~flags ("to_" ^ s)

  | Ptyp_constr (ident, _) ->
    let driver = module_name ~loc driver in
    expr_of_ident ~suffix:("_of_" ^ driver) ident

  | Ptyp_tuple cts -> begin
      let to_ts = List.map ~f:
          (fun ct -> deserialize_expr_of_type_descr ~loc ~driver ~flags ct.ptyp_desc ) cts
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
      [%expr
        let open Protocol_conv.Runtime in
        let of_funcs = [%e spec_expr ~loc (List.mapi ~f:(fun i v -> estring ~loc (sprintf "t%d" i), v) to_ts) ] in
        let constructor = [%e constructor] in
        [%e driver_func ~loc ~driver ~flags "to_tuple"] of_funcs constructor
      ]
    end
  | Ptyp_poly _ -> raise_errorf ~loc "Polymorphic variants not supported"
  | Ptyp_variant _ -> raise_errorf ~loc "Variant type descr not supported"
  | Ptyp_any
  | Ptyp_var _
  | Ptyp_arrow _
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_alias _
  | Ptyp_package _
  | Ptyp_extension _ -> raise_errorf ~loc "Unsupported type descr"


let field_names_of_record ~driver labels =
  List.map ~f:(fun label -> match Attribute.get (key_attrib ~driver) label with
      | Some name -> { loc=label.pld_loc; txt=name }
      | None -> label.pld_name
    ) labels
  (* Test is names collide *)
  |> List.fold_left ~init:[] ~f:(
    fun acc n->
      match List.find ~f:(fun f -> String.equal f.txt n.txt) acc with
      | Some _ ->
        raise_errorf ~loc:n.loc "Field name already in use: %s" n.txt
      | None -> n :: acc
  )
  |> List.rev_map ~f:(fun { loc; txt } -> estring ~loc txt)

let serialize_expr_of_tdecl ~loc ~driver ~flags tdecl =
  match tdecl.ptype_kind with
  | Ptype_abstract -> begin
      match tdecl.ptype_manifest with
      | Some core_type ->
        serialize_expr_of_type_descr ~loc ~driver ~flags core_type.ptyp_desc
      | None -> raise_errorf ~loc "Manifest is none"
    end
  | Ptype_variant constrs ->
    let mk_pattern core_types =
      List.mapi ~f:(fun i (core_type:core_type) ->
          ppat_var
            ~loc:core_type.ptyp_loc
            { loc = core_type.ptyp_loc; txt = sprintf "c%d" i }
        ) core_types
      |> ppat_tuple_opt ~loc
    in
    let mk_case = function
      | { pcd_args = Pcstr_record _; pcd_loc; _ } ->
        (* Should create a constructor that converts this into a standard record.
           But we dont have a name, and cannot use it outside the constr - so its hard... *)
        raise_errorf ~loc:pcd_loc "Anonymous records not supported"
      | { pcd_name; pcd_args = Pcstr_tuple core_types; pcd_loc=loc; _ } ->
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
              (serialize_expr_of_type_descr
                 ~driver
                 ~flags
                 ~loc core_type.ptyp_desc)
              [Nolabel, pexp_ident ~loc { loc; txt=Lident (sprintf "c%d" i) }]
          ) core_types
        in
        let rhs =
          [%expr ( [%e estring ~loc pcd_name.txt ],
                   [%e args |> list_expr ~loc] )]
        in
        case ~lhs ~guard:None ~rhs
    in
    [%expr
      [%e driver_func ~loc ~driver ~flags "of_variant" ]
        [%e pexp_function ~loc (List.map ~f:mk_case constrs) ]
    ]
  | Ptype_record labels ->
    (* Map field names. @key attribute takes priority, and test for name clashes *)
    let field_names = field_names_of_record ~driver labels in
    let field_ids = List.map ~f:(fun ld -> ld.pld_name) labels in
    let to_p =
      List.map ~f:(fun ld -> serialize_expr_of_type_descr ~loc ~driver ~flags ld.pld_type.ptyp_desc) labels
    in
    let arg_list =
      List.map3_exn ~f:(fun name id of_t ->
          [%expr [%e name],
                 [%e pexp_apply ~loc of_t
                     [ Nolabel, pexp_ident ~loc { loc; txt=Lident id.txt }]
                 ]
          ]
        ) field_names field_ids to_p
      |> list_expr ~loc
    in
    let body = pexp_apply ~loc (driver_func ~loc ~driver ~flags "of_record") [Nolabel, arg_list] in
    pexp_fun ~loc Nolabel None
      (ppat_record ~loc
         (List.map ~f:(fun id ->
              { loc; txt=Lident id.txt }, ppat_var ~loc id) field_ids)
         Closed)
      body

  | Ptype_open -> raise_errorf ~loc "open types not supported"

let default_case ~loc =
  let efail = pexp_apply ~loc (pexp_ident ~loc { loc; txt=Lident "failwith" })
      [ Nolabel, estring ~loc "Unknown variant or arity error" ]
  in
  case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:efail

let deserialize_expr_of_tdecl ~loc ~driver ~flags tdecl =
  match tdecl.ptype_kind with
  | Ptype_abstract -> begin
      match tdecl.ptype_manifest with
      | Some core_type ->
        deserialize_expr_of_type_descr ~loc ~driver ~flags core_type.ptyp_desc
      | None -> raise_errorf ~loc "Manifest is none"
    end
  | Ptype_variant constrs ->
    let mk_case = function
      | { pcd_args = Pcstr_record _; pcd_loc; _ } ->
        raise_errorf ~loc:pcd_loc "Anonymous records not supported"
      | { pcd_name; pcd_args = Pcstr_tuple core_types; pcd_loc=loc; _ } ->
        (* val: to_variant: ((string * t list) -> 'a) -> t -> 'a *)
        let lhs =
          let pcstr s pat = ppat_construct ~loc { loc; txt=Lident s } pat in
          core_types
          |> List.rev_mapi ~f:(fun i _ -> sprintf "c%d" i)
          |> List.map ~f:(fun s -> { loc; txt=s })
          |> List.fold_left ~init:(pcstr "[]" None)
            ~f:(fun acc var ->
                pcstr "::" (Some (ppat_tuple ~loc
                                    [ppat_var ~loc var; acc]))
              )
          |> fun p -> ppat_tuple ~loc [ ppat_constant ~loc (Pconst_string (pcd_name.txt, None)); p ]
        in
        let rhs =
          (* A function c1 c2 c3 -> A (to_t c1, to_t c2, to_t c3) *)
          let of_p var core_type =
            let e = deserialize_expr_of_type_descr ~loc ~driver ~flags core_type in
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
      [%e driver_func ~loc ~driver ~flags "to_variant" ]
        [%e pexp_function ~loc (List.map ~f:mk_case constrs |> fun ps -> ps @ [default_case ~loc]) ]
    ]
  | Ptype_record labels ->
    (* Map field names. @key attribute takes priority, and test for name clashes *)
    let field_names = field_names_of_record ~driver labels in
    let field_ids = List.map ~f:(fun ld -> ld.pld_name) labels in

    (* From needs constructor: fun a -> { a } *)
    let constructor =
      let record = pexp_record ~loc
          (List.map ~f:(fun id -> let id = { loc; txt=Lident id.txt } in id, pexp_ident ~loc id) field_ids)
          None
      in
      List.fold_right ~init:record ~f:(fun field expr ->
          pexp_fun ~loc Nolabel None (ppat_var ~loc field) expr
        ) field_ids
    in
    let of_p = List.map ~f:(
        fun ld -> deserialize_expr_of_type_descr ~loc ~driver ~flags ld.pld_type.ptyp_desc
      ) labels
    in

    [%expr
      let open Protocol_conv.Runtime in
      let of_funcs = [%e spec_expr ~loc (List.zip_exn field_names of_p)] in
      let constructor = [%e constructor] in
      [%e driver_func ~loc ~driver ~flags "to_record"] of_funcs constructor
    ]

  | Ptype_open -> raise_errorf ~loc "open types not supported"

let serialize_function_name ~loc ~(driver:longident) name =
  sprintf "%s_to_%s" name.txt (module_name ~loc driver) |> Located.mk ~loc

let deserialize_function_name ~loc ~(driver:longident) name =
  sprintf "%s_of_%s" name.txt (module_name ~loc driver) |> Located.mk ~loc

let pstr_value_of_funcs ~loc elements =
  List.map ~f:(fun (name, expr) ->
      let pat = ppat_var ~loc name in
      value_binding ~loc ~pat ~expr
    ) elements
  |> pstr_value ~loc Recursive

let rec_func ~loc = {loc; txt="__protocol_recursive_function"}, [%expr fun () -> __protocol_recursive_function ()]

let to_protocol_str_type_decls ~loc ~path:_ (_rec_flag, tydecls) driver flags =
  let driver = ident_of_module ~loc driver in
  [ pstr_value_of_funcs ~loc
      ( rec_func ~loc :: List.map
          ~f:(fun tdecl ->
              let name = tdecl.ptype_name in
              ( serialize_function_name ~loc ~driver name,
                [%expr fun t -> [%e serialize_expr_of_tdecl ~loc ~driver ~flags tdecl] t]
              )
            ) tydecls
      )
  ]

let of_protocol_str_type_decls ~loc ~path:_ (_rec_flag, tydecls) driver flags =
  let driver = ident_of_module ~loc driver in
  [ pstr_value_of_funcs ~loc
      ( rec_func ~loc :: List.map
          ~f:(fun tdecl ->
              let name = tdecl.ptype_name in
              ( deserialize_function_name ~loc ~driver name,
                [%expr fun t -> [%e deserialize_expr_of_tdecl ~loc ~driver ~flags tdecl] t]
              )
            ) tydecls
      )
  ]

let protocol_str_type_decls ~loc ~path (rec_flag, tydecls) driver flags =
  to_protocol_str_type_decls ~loc ~path (rec_flag, tydecls) driver flags @
  of_protocol_str_type_decls ~loc ~path (rec_flag, tydecls) driver flags

let mk_typ ~loc name =
  ptyp_constr ~loc (Located.mk ~loc (Longident.parse name)) []

let to_protocol_sig_type_decls ~loc ~path:_ (_rec_flag, tydecls) (driver:module_expr option) =
  let driver = ident_of_module ~loc driver in
  List.map ~f:(fun tydecl ->
      let name = tydecl.ptype_name in
      let result = string_of_ident (Ldot (driver, "t")) in
      let to_p = serialize_function_name ~loc ~driver name  in
      let to_type = [%type: [%t mk_typ ~loc name.txt] -> [%t mk_typ ~loc result]] in
      psig_value ~loc (value_description ~loc ~name:to_p ~type_:to_type ~prim:[])
    ) tydecls

let of_protocol_sig_type_decls ~loc ~path:_ (_rec_flag, tydecls) (driver:module_expr option) =
  let driver = ident_of_module ~loc driver in
  List.map ~f:(fun tydecl ->
      let name = tydecl.ptype_name in
      let result = string_of_ident (Ldot (driver, "t")) in
      let of_p = deserialize_function_name ~loc ~driver name  in
      let of_type = [%type: [%t mk_typ ~loc result] -> [%t mk_typ ~loc name.txt]] in
      psig_value ~loc (value_description ~loc ~name:of_p ~type_:of_type ~prim:[])
    ) tydecls

let protocol_sig_type_decls ~loc ~path (rec_flag, tydecls) (driver:module_expr option) =
  to_protocol_sig_type_decls ~loc ~path (rec_flag, tydecls) driver @
  of_protocol_sig_type_decls ~loc ~path (rec_flag, tydecls) driver

let () =
  let driver = Type_conv.Args.(arg "driver" (pexp_pack __)) in
  let flags = Type_conv.Args.(arg "flags" __) in
  Type_conv.add "protocol"
    ~str_type_decl:(Type_conv.Generator.make Type_conv.Args.(empty +> driver +> flags) protocol_str_type_decls)
    ~sig_type_decl:(Type_conv.Generator.make Type_conv.Args.(empty +> driver) protocol_sig_type_decls)
  |> Type_conv.ignore;

  Type_conv.add "of_protocol"
    ~str_type_decl:(Type_conv.Generator.make Type_conv.Args.(empty +> driver +> flags) of_protocol_str_type_decls)
    ~sig_type_decl:(Type_conv.Generator.make Type_conv.Args.(empty +> driver) of_protocol_sig_type_decls)
  |> Type_conv.ignore;

  Type_conv.add "to_protocol"
    ~str_type_decl:(Type_conv.Generator.make Type_conv.Args.(empty +> driver +> flags) to_protocol_str_type_decls)
    ~sig_type_decl:(Type_conv.Generator.make Type_conv.Args.(empty +> driver) to_protocol_sig_type_decls)
  |> Type_conv.ignore;
