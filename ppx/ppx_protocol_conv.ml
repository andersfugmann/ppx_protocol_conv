open Ppxlib
open Ast_builder.Default
open !Printf
open Base

type t = {
  driver: longident;
  field_key:     (label_declaration, string) Attribute.t;
  constr_key:    (constructor_declaration, string) Attribute.t;
  constr_name:   (constructor_declaration, string) Attribute.t;
  variant_key:   (row_field, string) Attribute.t;
  variant_name:  (row_field, string) Attribute.t;
  field_default: (label_declaration, expression) Attribute.t;
}

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

let pexp_ident_string_loc { loc; txt } =
  pexp_ident ~loc {loc; txt = Lident txt}

let driver_func t ~loc name =
  pexp_ident ~loc { loc; txt = Ldot (t.driver, name) }

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

(** Test if a type is considered primitive *)
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

let protocol_ident dir driver { loc; txt } =
  let prefix, suffix = match dir with
    | `Serialze -> "to", ""
    | `Deserialize -> "of" , "_exn"
  in
  (* Match the name of the type *)
  let driver_name = module_name ~loc driver in
  let txt = match txt with
    | Lident "t" -> Lident (sprintf "%s_%s%s" prefix driver_name suffix)
    | Lident name -> Lident (sprintf "%s_%s_%s%s" name prefix driver_name suffix)
    | Ldot (l, "t") -> Ldot (l, sprintf "%s_%s%s" prefix driver_name suffix)
    | Ldot (l, name) -> Ldot (l, sprintf "%s_%s_%s%s" name prefix driver_name suffix)
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
let rec serialize_record t ~loc labels =
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
        { loc; txt=Lident id.txt }, ppat_var ~loc { loc; txt = "r_" ^ id.txt})
        (List.map ~f:(fun ld -> ld.pld_name) labels)
     ) Closed,
  List.map ~f:(fun label -> Nolabel, pexp_ident ~loc { loc; txt=Lident ("r_" ^ label.pld_name.txt) }) labels

and serialize_tuple t ~loc core_types =
  let spec =
    List.map core_types ~f:(fun ct -> serialize_expr_of_type_descr t ~loc ct.ptyp_desc)
    |> slist_expr ~loc
  in
  let ids = List.mapi core_types ~f:(fun i _ -> sprintf "t%d" i) in
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

and serialize_variant t ~loc type_ ~name ~alias pcstr  =
  let ppat_constr ~loc name pattern = match type_ with
    | `Variant -> ppat_variant ~loc name pattern
    | `Construct -> ppat_construct ~loc {loc; txt = Lident name} pattern
  in
  let mk_pattern core_types =
    List.mapi ~f:(fun i (core_type:core_type) ->
        ppat_var
          ~loc:core_type.ptyp_loc
          { loc = core_type.ptyp_loc; txt = sprintf "c%d" i }
      ) core_types
    |> ppat_tuple_opt ~loc
  in
  match pcstr with
  | Pcstr_record labels ->
    let spec, patt, args = serialize_record t ~loc labels in
    let patt_tuple = List.map ~f:(fun label -> ppat_var ~loc { loc; txt = "r_" ^ label.pld_name.txt}) labels |> ppat_tuple ~loc in
    let f = [%expr
      let f = [%e driver_func t ~loc "of_record"] [%e spec] in
      let f [%p patt_tuple] = [%e pexp_apply ~loc (pexp_ident ~loc {loc; txt = Lident "f"}) args] in
      let spec =
        let open Protocol_conv.Runtime.Tuple_out in
        (Cons (f, Nil))
      in
      [%e driver_func t ~loc "of_variant"] [%e estring ~loc alias ]spec
    ]
    in

    let f_name = { loc; txt = sprintf "_%s_of_record_" name } in
    let lhs = ppat_constr ~loc name (Some patt) in
    let rhs = pexp_apply ~loc (pexp_ident_string_loc f_name) [Nolabel, pexp_tuple ~loc (List.map ~f:snd args)] in
    let binding = value_binding ~loc ~pat:{ppat_desc = Ppat_var f_name; ppat_loc = loc; ppat_attributes=[]} ~expr:f in
    binding, case ~lhs ~guard:None ~rhs
  | Pcstr_tuple core_types ->
    let f_name = { loc; txt = sprintf "_%s_of_tuple" name } in
    let f =
      let spec =
        List.map core_types ~f:(fun ct -> serialize_expr_of_type_descr t ~loc ct.ptyp_desc)
        |> slist_expr ~loc
      in
      [%expr [%e driver_func t ~loc "of_variant"]
          [%e estring ~loc alias] Protocol_conv.Runtime.Tuple_out.([%e spec])
      ]
    in
    let binding = value_binding ~loc ~pat:{ppat_desc = Ppat_var f_name; ppat_loc = loc; ppat_attributes=[]} ~expr:f in

    let lhs = ppat_constr ~loc name (mk_pattern core_types) in
    let args =
      List.mapi ~f:(fun i _-> pexp_ident ~loc { loc; txt=Lident (sprintf "c%d" i) }) core_types
    in
    let rhs =
      pexp_apply ~loc (pexp_ident_string_loc f_name) (List.map ~f:(fun a -> (Nolabel, a)) args)
    in
    binding, case ~lhs ~guard:None ~rhs

and serialize_expr_of_tdecl t ~loc tdecl =
  match tdecl.ptype_kind with
  | Ptype_abstract -> begin
      match tdecl.ptype_manifest with
      | Some core_type ->
        serialize_expr_of_type_descr t ~loc core_type.ptyp_desc
      | None -> raise_errorf ~loc "Opaque types are not supported."
    end
  | Ptype_variant constrs ->
    test_constructor_mapping t constrs;
    let bindings, cases =
      List.map ~f:(fun ({ pcd_name; pcd_args = pcstr; pcd_loc=loc; _ } as constr) ->
          let alias = match get_constr_name t constr with
            | Some key -> key
            | None -> pcd_name.txt
          in
          serialize_variant t ~loc `Construct ~name:pcd_name.txt ~alias pcstr
        ) constrs
      |> List.unzip
    in
    pexp_let ~loc Nonrecursive bindings @@ pexp_function ~loc cases

  | Ptype_record labels ->
    let spec, patt, args = serialize_record t ~loc labels in
    [%expr
      let _of_record = [%e (driver_func t ~loc "of_record")] [%e spec] in
      fun [%p patt] ->
        [%e pexp_apply ~loc [%expr _of_record] args]
    ]
  | Ptype_open -> raise_errorf ~loc "Extensible variant types not supported"

(** Serialization expression for a given type *)
and serialize_expr_of_type_descr t ~loc = function
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
    let func = protocol_ident `Serialze t.driver ident in
    pexp_apply ~loc func args

  | Ptyp_tuple core_types ->
    serialize_tuple ~loc t core_types
  | Ptyp_poly _      -> raise_errorf ~loc "Polymorphic variants not supported"
  | Ptyp_variant (rows, _closed, None) ->
    test_row_mapping t rows;
    let bindings, cases =
      List.map ~f:(function
          | Rinherit _ -> raise_errorf ~loc "Inherited types not supported"
          | Rtag (name, _attributes, _bool, core_types) as row ->
            let alias = match get_variant_name t row with
              | Some key -> key
              | None -> name.txt
            in
            serialize_variant t ~loc `Variant ~name:name.txt ~alias (Pcstr_tuple core_types)
        ) rows
      |> List.unzip
    in
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


(*=========== Deserialization functions ==============*)
(* Create a tuple deserialization function *)

let rec deserialize_record t ~loc ?map_result labels =
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
  ([%expr Protocol_conv.Runtime.Record_in.([%e spec])], constructor)

and deserialize_tuple t ~loc ~constr cts =
  let constructor =
    let ids = List.mapi ~f:(fun i _ -> { loc; txt=Lident (sprintf "x%d" i) }) cts in
    let tuple =
      pexp_tuple ~loc (List.map ~f:(pexp_ident ~loc) ids) |> constr
    in
    List.fold_right ~init:tuple ~f:(fun id expr ->
        pexp_fun ~loc Nolabel None (ppat_var ~loc (string_of_ident_loc id)) expr
      ) ids
  in
  let slist =
    List.map ~f:(fun ct -> deserialize_expr_of_type_descr t ~loc ct.ptyp_desc) cts
  in
  (slist_expr ~loc slist, constructor)

and deserialize_variant t ~loc type_ ~name pcstrs =
  let pexp_constr ~loc name = match type_ with
    | `Construct -> pexp_construct ~loc { loc; txt = Lident name }
    | `Variant -> pexp_variant ~loc name
  in
  match pcstrs with
    | Pcstr_record labels ->
        let map_result x = pexp_constr ~loc name (Some x) in
        let spec, constr = deserialize_record t ~loc ~map_result labels in (* Why not just create the function right there!!!! *)
        let f = pexp_apply ~loc (driver_func t ~loc "to_record") [Nolabel, spec; Nolabel, constr] in
        let spec = [%expr Protocol_conv.Runtime.Tuple_in.(Cons ([%e f], Nil))] in
        let constr = [%expr fun x -> x] in
        spec, constr
    | Pcstr_tuple [] -> [%expr Protocol_conv.Runtime.Tuple_in.Nil], pexp_constr ~loc name None
    | Pcstr_tuple core_types ->
      let spec =
        List.map ~f:(fun ct -> deserialize_expr_of_type_descr t ~loc ct.ptyp_desc) core_types
        |> slist_expr ~loc
        |> fun e -> [%expr Protocol_conv.Runtime.Tuple_in.( [%e e] ) ]
      in
      let constructor =
        let arg_names = List.mapi ~f:(fun i _ -> {loc; txt = Lident (sprintf "v%d" i)}) core_types in
        let body =
          pexp_tuple ~loc (List.map ~f:(pexp_ident ~loc) arg_names)
          |> Option.some
          |> pexp_constr ~loc name
        in
        List.fold_right ~init:body ~f:(fun id expr ->
            pexp_fun ~loc Nolabel None (ppat_var ~loc (string_of_ident_loc id)) expr
          ) arg_names
      in
      spec, constructor


and deserialize_expr_of_tdecl t ~loc tdecl =
  match tdecl.ptype_kind with
  | Ptype_abstract -> begin
      match tdecl.ptype_manifest with
      | Some core_type ->
        deserialize_expr_of_type_descr t ~loc core_type.ptyp_desc
      | None -> raise_errorf ~loc "Manifest is none"
    end
  | Ptype_variant constrs ->
    test_constructor_mapping t constrs;
    let mk_elem ({ pcd_name; pcd_args; pcd_loc=loc; _ } as constr) =
      let ser_name = match get_constr_name t constr with
        | Some key -> key
        | None -> pcd_name.txt
      in
      let spec, constr = deserialize_variant t ~loc `Construct ~name:pcd_name.txt pcd_args in
      (ser_name, spec, constr)
    in
    let spec =
      List.map ~f:mk_elem constrs
      |> List.map ~f:(fun (name, spec, constr) ->
          [%expr Protocol_conv.Runtime.Variant_in.Variant ([%e estring ~loc name], [%e spec], [%e constr])]
        )
      |> list_expr ~loc
    in
    pexp_apply ~loc (driver_func t ~loc "to_variant") [Nolabel, spec]

  | Ptype_record labels ->
    let spec, constructor = deserialize_record t ~loc labels in
    pexp_apply ~loc (driver_func t ~loc "to_record") [Nolabel, spec; Nolabel, constructor]

  | Ptype_open -> raise_errorf ~loc "Extensible variant types not supported"


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
    let func = protocol_ident `Deserialize t.driver ident in
    pexp_apply ~loc func args

  | Ptyp_tuple cts ->
    let (spec, constructor) = deserialize_tuple t ~constr:Fn.id ~loc cts in
    [%expr
      let spec = Protocol_conv.Runtime.Tuple_in.([%e spec ]) in
      let constructor = [%e constructor] in
      [%e driver_func t ~loc "to_tuple"] spec constructor
    ]
  | Ptyp_poly _ ->
    raise_errorf ~loc "Polymorphic variants not supported"
  | Ptyp_variant (_rows, _closed, Some _) ->
    raise_errorf ~loc "Variant with some"

  | Ptyp_variant (rows, _closed, None) ->
    test_row_mapping t rows;
    let mk_elem = function
      | Rinherit _ -> raise_errorf ~loc "Inherited variant types not supported"
      | Rtag (name, _attributes, _bool, core_types) as row ->
        let ser_name = match get_variant_name t row with
          | Some key -> key
          | None -> name.txt
        in
        let spec, constr = deserialize_variant t ~loc `Variant ~name:name.txt (Pcstr_tuple core_types) in
        (ser_name, spec, constr)
    in
    let spec =
      List.map ~f:mk_elem rows
      |> List.map ~f:(fun (name, spec, constr) -> [%expr Protocol_conv.Runtime.Variant_in.Variant ([%e estring ~loc name], [%e spec], [%e constr])])
      |> list_expr ~loc
    in
    pexp_apply ~loc (driver_func t ~loc "to_variant") [Nolabel, spec]

  | Ptyp_var name -> pexp_ident ~loc { loc; txt = Lident ( sprintf "__param_of_%s" name) }

  | Ptyp_arrow _ -> raise_errorf ~loc "Functions not supported"
  | Ptyp_any
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_alias _
  | Ptyp_package _
  | Ptyp_extension _ -> raise_errorf ~loc "Unsupported type descr"

let serialize_function_name ~loc ~driver name =
  let prefix = match name.txt with
    | "t" -> ""
    | name -> name ^ "_"
  in
  sprintf "%sto_%s" prefix (module_name ~loc driver) |> Located.mk ~loc

let deserialize_function_name ?(as_result = false) ~loc ~driver name =
  let prefix = match name.txt with
    | "t" -> ""
    | name -> name ^ "_"
  in
  let suffix = match as_result with
    | true -> ""
    | false -> "_exn"
  in
  sprintf "%sof_%s%s" prefix (module_name ~loc driver) suffix |> Located.mk ~loc

let _deserialize_function_name_result ~loc ~driver name =
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

(** Test if a type references itself, in which case we cannot do eager evaluation,
    and we create a reference cell to hold the evaluated function value for later.
    In this case, we only modify the reference cell once, at which point the closure will be moved to the heap,
    but thats ok, because all the closures will end up there anyways.

    Return a function which will determin which of the tydecls references other types in the list of tydecls,
    so only functions which needs this 'recursion optimization hack' will be wrapped.
*)
let is_recursive tydecls = function
  | Nonrecursive -> fun _ -> false
  | Recursive ->
    let names = List.map ~f:(fun { ptype_name = { txt=name; _}; _ } -> name) tydecls in
    fun { ptype_kind; ptype_params = ctvl; ptype_manifest; _} ->
      is_recursive names ptype_kind ||
      List.exists ~f:(fun (ct, _var) -> is_recursive_ct names ct) ctvl ||
      Option.value_map ptype_manifest ~default:false ~f:(is_recursive_ct names)

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

let type_of_of_func ~loc driver ~as_result tydecl =
  let typ = mk_typ ~loc tydecl in
  let result_type = match as_result with
    | false -> typ
    | true ->
      let error_typ = ptyp_constr ~loc { loc; txt = Ldot (driver, "error") } [] in
      ptyp_constr ~loc { loc; txt = Ldot (Ldot (Lident "Protocol_conv", "Runtime"), "result") } [typ; error_typ]
  in
  [%type: [%t ptyp_constr ~loc { loc; txt = Ldot (driver, "t")} [] ] -> [%t result_type]]

let serialization_signature ~loc ~as_sig driver tdecl =
  let type_of = type_of_to_func ~loc driver tdecl in
  let params =
    List.filter_map
      ~f:(function ({ ptyp_desc = Ptyp_var s; _ }, _variance) -> Some s
                 | _ -> None) tdecl.ptype_params
  in
  let signature =
    List.fold_right params ~init:type_of ~f:(fun name acc ->
        let typ =
          ptyp_arrow ~loc Nolabel
            (ptyp_var ~loc name)
            (ptyp_constr ~loc {loc; txt = Ldot (driver, "t")} [])
        in
        ptyp_arrow ~loc Nolabel typ acc
      )
  in
  match as_sig with
  | true -> signature
  | false -> ptyp_poly ~loc (List.map ~f:(fun txt -> { loc; txt }) params) signature

let deserialization_signature ~loc ~as_sig ~as_result driver tdecl =
  let type_of = type_of_of_func ~loc ~as_result driver tdecl in
  let params =
    List.filter_map
      ~f:(function ({ ptyp_desc = Ptyp_var s; _ }, _variance) -> Some s
                 | _ -> None) tdecl.ptype_params
  in
  let signature =
    List.fold_right params ~init:type_of ~f:(fun name acc ->
        let typ =
          ptyp_arrow ~loc Nolabel
            (ptyp_constr ~loc {loc; txt = Ldot (driver, "t")} [])
            (ptyp_var ~loc name)
        in
        ptyp_arrow ~loc Nolabel typ acc
      )
  in
  match as_sig with
  | true -> signature
  | false -> ptyp_poly ~loc (List.map ~f:(fun txt -> { loc; txt }) params) signature

(** Cache intermediate result. Unfortunatly we are not allowed to create a spec, so we cache in an option reference. *)
let make_recursive ~loc (e : expression) = function
  | false -> e
  | true ->
    [%expr
      ( let f = ref None in
            (fun t -> match !f with
               | None ->
                 let f' = [%e e] in f := Some f'; f' t
               | Some f -> f t
            ))
    ]


let to_protocol_str_type_decls t rec_flag ~loc tydecls =
  let (defs, is_recursive) =
    let is_recursive_f = is_recursive tydecls rec_flag in
    List.fold_right ~init:([], false)
      ~f:(fun tdecl (acc, acc_recursive) ->
          let is_recursive = is_recursive_f tdecl in
          let to_p = serialize_function_name ~loc ~driver:t.driver tdecl.ptype_name in
          let expr = make_recursive ~loc (serialize_expr_of_tdecl t ~loc tdecl) is_recursive in
          let expr_param =
            List.fold_right ~init:expr ~f:(fun (ct, _variance) expr ->
                let patt = Ast_helper.Pat.var ~loc (name_of_core_type ~prefix:"to" ct) in
                [%expr fun [%p patt] -> [%e expr] ]
              ) tdecl.ptype_params
          in
          let signature = serialization_signature ~as_sig:false t.driver ~loc tdecl in
          (to_p, Some signature, expr_param) :: acc, (is_recursive || acc_recursive)
        ) tydecls
  in
  pstr_value_of_funcs ~loc (if is_recursive then Recursive else Nonrecursive) defs
  |> fun x -> [ x ]

let of_protocol_str_type_decls t rec_flag ~loc tydecls =
  let expr_param ~loc tdecl expr =
    List.fold_right ~init:expr ~f:(fun (ct, _variance) expr ->
        let patt = Ast_helper.Pat.var ~loc (name_of_core_type ~prefix:"of" ct) in
        [%expr fun [%p patt] -> [%e expr] ])
      tdecl.ptype_params
  in
  let result_expr ~loc tdecl of_p =
    let expr =
      let type_params = List.map
          ~f:(fun (ct, _variance) ->
              pexp_ident ~loc { loc; txt = Lident (name_of_core_type ~prefix:"of" ct).txt })
          tdecl.ptype_params
      in
      (*
      let args =
        type_params @ [ pexp_ident ~loc { loc; txt = Lident "t"} ]
        |> List.map ~f:(fun e -> (Nolabel, e))
      in
      pexp_apply ~loc (pexp_ident ~loc { loc; txt = Lident of_p.txt}) args

*)
      let args =
        type_params
        |> List.map ~f:(fun e -> (Nolabel, e))
      in
      pexp_apply ~loc (pexp_ident ~loc { loc; txt = Lident of_p.txt}) args
    in
    pexp_apply ~loc (driver_func t ~loc "try_with") [Nolabel, expr]
  in
  let (defs, err_defs, is_recursive) =
    let is_recursive_f = is_recursive tydecls rec_flag in
    List.fold_right ~init:([], [], false)
      ~f:(fun tdecl (defs, err_defs, acc_recursive) ->
          let is_recursive = is_recursive_f tdecl in
          let of_p = deserialize_function_name ~loc ~driver:t.driver tdecl.ptype_name in
          let expr = make_recursive ~loc (deserialize_expr_of_tdecl t ~loc tdecl) is_recursive in
          let signature = deserialization_signature ~as_sig:false ~as_result:false t.driver ~loc tdecl in

          let of_p_result = deserialize_function_name ~as_result:true ~loc ~driver:t.driver tdecl.ptype_name in
          let result_expr = result_expr ~loc tdecl of_p in
          let result_sig = deserialization_signature ~as_sig:false ~as_result:true t.driver ~loc tdecl in

          (of_p, Some signature, expr_param ~loc tdecl expr) :: defs,
          (of_p_result, Some result_sig, expr_param ~loc tdecl result_expr) :: err_defs,
          (is_recursive || acc_recursive)
        ) tydecls
  in
  [
    pstr_value_of_funcs ~loc (if is_recursive then Recursive else Nonrecursive) defs;
    pstr_value_of_funcs ~loc Nonrecursive err_defs;
  ]

let protocol_str_type_decls t rec_flag ~loc tydecls =
  to_protocol_str_type_decls t rec_flag ~loc tydecls @
  of_protocol_str_type_decls t rec_flag ~loc tydecls

let to_protocol_sig_type_decls ~loc ~path:_ (_rec_flag, tydecls) (driver:module_expr option) =
  let driver = ident_of_module ~loc driver in
  List.concat_map ~f:(fun tydecl ->
      let signature = serialization_signature ~as_sig:true ~loc driver tydecl in
      let to_p = serialize_function_name ~loc ~driver tydecl.ptype_name in
      psig_value ~loc (value_description ~loc ~name:to_p ~type_:signature ~prim:[]) :: []
    ) tydecls

let of_protocol_sig_type_decls ~loc ~path:_ (_rec_flag, tydecls) (driver:module_expr option) =
  let driver = ident_of_module ~loc driver in
  List.concat_map ~f:(fun tydecl ->
      let of_p = deserialize_function_name ~loc ~driver tydecl.ptype_name  in
      let signature = deserialization_signature ~as_sig:true ~as_result:false ~loc driver tydecl in

      let of_p_result = deserialize_function_name ~as_result:true ~loc ~driver tydecl.ptype_name in
      let result_sig = deserialization_signature ~as_sig:true ~as_result:true driver ~loc tydecl in

      [
        psig_value ~loc (value_description ~loc ~name:of_p ~type_:signature ~prim:[]);
        psig_value ~loc (value_description ~loc ~name:of_p_result ~type_:result_sig ~prim:[]);
      ]
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
