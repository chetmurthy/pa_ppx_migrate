
module SRC = All_ast.Ast_4_08
module DST = All_ast.Ast_4_07

let src_loc_none =
  let open SRC.Lexing in
  let open SRC.Location in
  let loc = {
    pos_fname = "";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }

let dst_loc_none =
  let open DST.Lexing in
  let open DST.Location in
  let loc = {
    pos_fname = "";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }

let wrap_loc inh v =
  let loc = match inh with
      None -> src_loc_none
    | Some loc -> loc in
  let open SRC.Location in
  { txt = v ; loc = loc }

let map_loc f v =
  let open SRC.Location in
  { txt = f v.txt ; loc = v.loc }

let unwrap_loc v = v.SRC.Location.txt

exception Migration_error of string * SRC.Location.t option

let migration_error location feature =
  raise (Migration_error (feature, location))

let _migrate_list subrw0 __dt__ __inh__ l =
  List.map (subrw0 __dt__ __inh__) l

type lexing_position = [%import: All_ast.Ast_4_08.Lexing.position]
and location_t = [%import: All_ast.Ast_4_08.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_08.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_08.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_08.Asttypes.label]
and arg_label = [%import: All_ast.Ast_4_08.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_4_08.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_08.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_08.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_08.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_08.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_08.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_08.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_08.Asttypes.variance]
and constant =  [%import: All_ast.Ast_4_08.Parsetree.constant]
and location_stack = [%import: All_ast.Ast_4_08.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_08.Parsetree.attribute
    [@with Asttypes.loc := location_loc
         ; Location.t := location_t
    ]
]
and extension = [%import: All_ast.Ast_4_08.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_08.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_08.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_08.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_08.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_08.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_08.Parsetree.row_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and row_field_desc = [%import: All_ast.Ast_4_08.Parsetree.row_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and object_field = [%import: All_ast.Ast_4_08.Parsetree.object_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and object_field_desc = [%import: All_ast.Ast_4_08.Parsetree.object_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and pattern = [%import: All_ast.Ast_4_08.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_08.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_08.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_08.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_4_08.Parsetree.case]
and letop = [%import: All_ast.Ast_4_08.Parsetree.letop]
and binding_op = [%import: All_ast.Ast_4_08.Parsetree.binding_op
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and value_description = [%import: All_ast.Ast_4_08.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_08.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_08.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_08.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_08.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_4_08.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_4_08.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
         ; Location.t := location_t
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_08.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_exception = [%import: All_ast.Ast_4_08.Parsetree.type_exception
    [@with Location.t := location_t ;]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_08.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_08.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_08.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_signature = [%import: All_ast.Ast_4_08.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_08.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_08.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ; Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_08.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_08.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_08.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_08.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_08.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_structure = [%import: All_ast.Ast_4_08.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_08.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_08.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
         ; Asttypes.label := label
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_08.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_08.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_08.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_08.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and signature = [%import: All_ast.Ast_4_08.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_08.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_08.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_4_08.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_substitution = [%import: All_ast.Ast_4_08.Parsetree.module_substitution
    [@with Longident.t := longident_t
         ; Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_08.Parsetree.module_type_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and 'a open_infos = [%import: 'a All_ast.Ast_4_08.Parsetree.open_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
    ]
]
and open_description = [%import: All_ast.Ast_4_08.Parsetree.open_description
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and open_declaration = [%import: All_ast.Ast_4_08.Parsetree.open_declaration]
and 'a include_infos = [%import: 'a All_ast.Ast_4_08.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_08.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_08.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_08.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_08.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_08.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_08.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_08.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_08.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_08.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_08.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and out_name = [%import: All_ast.Ast_4_08.Outcometree.out_name]
and out_ident = [%import: All_ast.Ast_4_08.Outcometree.out_ident]
and out_string = [%import: All_ast.Ast_4_08.Outcometree.out_string]
and out_attribute = [%import: All_ast.Ast_4_08.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_4_08.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_08.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_08.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_08.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_08.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_08.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_08.Outcometree.out_sig_item]
and out_type_decl = [%import: All_ast.Ast_4_08.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag]
]
and out_extension_constructor = [%import: All_ast.Ast_4_08.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_08.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_4_08.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_4_08.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_08.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_08.Outcometree.out_phrase]


[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = All_ast.Ast_4_08
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = All_ast.Ast_4_08.Asttypes
      ; dstmod = DST.Asttypes
      ; types = [
          arg_label
        ; closed_flag
        ; direction_flag
        ; label
        ; mutable_flag
        ; override_flag
        ; private_flag
        ; rec_flag
        ; variance
        ; virtual_flag
        ]
      }
      ; {
        srcmod = All_ast.Ast_4_08.Parsetree
      ; dstmod = DST.Parsetree
      ; types = [
          attributes
        ; case
        ; class_declaration
        ; class_description
        ; class_expr
        ; class_field
        ; class_field_desc
        ; class_field_kind
        ; class_signature
        ; class_structure
        ; class_type
        ; class_type_declaration
        ; class_type_field
        ; class_type_field_desc
        ; constant
        ; constructor_arguments
        ; constructor_declaration
        ; core_type_desc
        ; extension
        ; extension_constructor
        ; extension_constructor_kind
        ; include_declaration
        ; include_description
        ; label_declaration
        ; location_stack
        ; module_binding
        ; module_declaration
        ; module_expr
        ; module_expr_desc
        ; module_type
        ; module_type_declaration
        ; module_type_desc
        ; package_type
        ; payload
        ; pattern_desc
        ; signature
        ; signature_item
        ; structure
        ; structure_item
        ; type_declaration
        ; type_kind
        ; value_binding
        ; value_description
        ; with_constraint
        ]
      }
      ; {
        srcmod = All_ast.Ast_4_08.Outcometree
      ; dstmod = DST.Outcometree
      ; types = [
          out_attribute
        ; out_class_sig_item
        ; out_class_type
        ; out_extension_constructor
        ; out_ext_status
        ; out_ident
        ; out_module_type
        ; out_phrase
        ; out_rec_status
        ; out_sig_item
        ; out_string
        ; out_type_decl
        ; out_type_extension
        ; out_val_decl
        ; out_value
        ; out_variant
        ]
      }
      ]
    ; dispatchers = {
        migrate_option = {
          srctype = [%typ: 'a option]
        ; dsttype = [%typ: 'b option]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = (fun subrw __dt__ __inh__ x -> Option.map (subrw __dt__ __inh__) x)
        }
      ; migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_attribute = {
          srctype = [%typ: attribute]
        ; dsttype = [%typ: DST.Parsetree.attribute]
        ; code = fun __dt__ __inh__ { attr_name = attr_name;
                                      attr_payload = attr_payload;
                                      attr_loc = _ } ->
            (__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ attr_name,
             __dt__.migrate_payload __dt__ __inh__ attr_payload)
        }
      ; migrate_core_type = {
          srctype = [%typ: core_type]
        ; dsttype = [%typ: DST.Parsetree.core_type]
        ; inherit_code = Some ptyp_loc
        ; skip_fields = [ ptyp_loc_stack ]
        }
      ; migrate_row_field = {
          srctype = [%typ: row_field]
        ; dsttype = [%typ: DST.Parsetree.row_field]
        ; code = fun __dt__ __inh__ -> function
              { prf_desc = Rtag (ll, b, ctl);
                prf_loc = prf_loc ;
                prf_attributes = prf_attributes } ->
              let open DST.Parsetree in
              Rtag(__dt__.migrate_location_loc __dt__.migrate_label __dt__ __inh__ ll,
                   __dt__.migrate_attributes __dt__ __inh__ prf_attributes,
                   b,
                   List.map (__dt__.migrate_core_type __dt__ __inh__) ctl)

            | { prf_desc = Rinherit ct;
                prf_loc = prf_loc ;
                prf_attributes = prf_attributes } ->
              let open DST.Parsetree in
              Rinherit (__dt__.migrate_core_type __dt__ __inh__ ct)
        }
      ; migrate_object_field = {
          srctype = [%typ: object_field]
        ; dsttype = [%typ: DST.Parsetree.object_field]
        ; code = fun __dt__ __inh__ -> function
              { pof_desc = Otag(ll, ct);
                pof_loc = pof_loc;
                pof_attributes = pof_attributes } ->
              let open DST.Parsetree in
              Otag(__dt__.migrate_location_loc __dt__.migrate_label __dt__ __inh__ ll,
                   __dt__.migrate_attributes __dt__ __inh__ pof_attributes,
                   __dt__.migrate_core_type __dt__ __inh__ ct)

            | { pof_desc = Oinherit ct;
                pof_loc = pof_loc;
                pof_attributes = pof_attributes } ->
              let open DST.Parsetree in
              Oinherit (__dt__.migrate_core_type __dt__ __inh__ ct)
        }
      ; migrate_pattern = {
          srctype = [%typ: pattern]
        ; dsttype = [%typ: DST.Parsetree.pattern]
        ; inherit_code = Some ppat_loc
        ; skip_fields = [ ppat_loc_stack ]
        }
      ; migrate_expression = {
          srctype = [%typ: expression]
        ; dsttype = [%typ: DST.Parsetree.expression]
        ; inherit_code = Some pexp_loc
        ; skip_fields = [ pexp_loc_stack ]
        }
      ; migrate_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.Parsetree.expression_desc]
        ; custom_branches_code = function
            | Pexp_open ({ popen_expr = { pmod_desc = Pmod_ident ll;
                                          pmod_loc = pmod_loc ;
                                          pmod_attributes = pmod_attributes } ;
                           popen_override = popen_override;
                           popen_loc = popen_loc ;
                           popen_attributes = attributes }, e) ->
              let open DST.Parsetree in
              Pexp_open(__dt__.migrate_override_flag __dt__ __inh__ popen_override,
                        __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ ll,
                        __dt__.migrate_expression __dt__ __inh__ e)
            | Pexp_open ({ popen_expr = _ ; }, _) ->
              migration_error __inh__ "Pexp_open:longident"
            | Pexp_letop _ ->
              migration_error __inh__ "Pexp_letop"
        }
      ; migrate_type_extension = {
          srctype = [%typ: type_extension]
        ; dsttype = [%typ: DST.Parsetree.type_extension]
        ; skip_fields = [ ptyext_loc ]
        }
      ; migrate_class_type_desc = {
          srctype = [%typ: class_type_desc]
        ; dsttype = [%typ: DST.Parsetree.class_type_desc]
        ; custom_branches_code = function
            | Pcty_open ({ popen_expr = ll;
                   popen_override = popen_override;
                   popen_loc = popen_loc;
                   popen_attributes = popen_attributes }, ct) ->
              let open DST.Parsetree in
              Pcty_open (__dt__.migrate_override_flag __dt__ __inh__ popen_override,
                         __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ ll,
                        __dt__.migrate_class_type __dt__ __inh__ ct)
        }
      ; migrate_class_expr_desc = {
          srctype = [%typ: class_expr_desc]
        ; dsttype = [%typ: DST.Parsetree.class_expr_desc]
        ; custom_branches_code = function
            Pcl_open
                ({ popen_expr = v_1;
                   popen_override = v_0 ;
                   popen_loc = popen_loc ;
                   popen_attributes = popen_attributes },
                 v_2) ->
              let open DST.Parsetree in
              Pcl_open (__dt__.migrate_override_flag __dt__ __inh__ v_0,
                        __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ v_1,
                        __dt__.migrate_class_expr __dt__ __inh__ v_2)
        }
      ; migrate_signature_item_desc = {
          srctype = [%typ: signature_item_desc]
        ; dsttype = [%typ: DST.Parsetree.signature_item_desc]
        ; custom_branches_code = function
            | Psig_exception {ptyexn_constructor = ptyexn_constructor} ->
              let open DST.Parsetree in
              Psig_exception (__dt__.migrate_extension_constructor __dt__ __inh__ ptyexn_constructor)
            | Psig_modsubst _ ->
              migration_error __inh__ "Psig_modsubst"
            | Psig_typesubst _ -> migration_error __inh__ "Psig_typesubst"
        }
      ; migrate_open_description = {
          srctype = [%typ: open_description]
        ; dsttype = [%typ: DST.Parsetree.open_description]
        ; inherit_code = Some popen_loc
        ; skip_fields = [ popen_expr ]
        ; custom_fields_code = {
            popen_lid = __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ popen_expr
          }
        }
      ; migrate_structure_item_desc = {
          srctype = [%typ: structure_item_desc]
        ; dsttype = [%typ: DST.Parsetree.structure_item_desc]
        ; custom_branches_code = function
            | Pstr_exception {ptyexn_constructor = ptyexn_constructor} ->
              let open DST.Parsetree in
              Pstr_exception (__dt__.migrate_extension_constructor __dt__ __inh__ ptyexn_constructor)
            | Pstr_open { popen_expr = { pmod_desc = Pmod_ident ll;
                                 pmod_loc = pmod_loc ;
                                 pmod_attributes = pmod_attributes } ;
                          popen_override = popen_override;
                          popen_loc = popen_loc;
                          popen_attributes = popen_attributes } ->
              let open DST.Parsetree in
              Pstr_open { popen_lid = __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ ll ;
                          popen_override = __dt__.migrate_override_flag __dt__ __inh__ popen_override ;
                          popen_loc = __dt__.migrate_location_t __dt__ __inh__ popen_loc ;
                          popen_attributes  = __dt__.migrate_attributes __dt__ __inh__ popen_attributes }
            | Pstr_open { popen_expr = _ ; } ->
              migration_error __inh__ "Pstr_open:longident"
        }
      ; migrate_printer = {
          srctype = [%typ: (Format.formatter -> unit)]
        ; dsttype = [%typ: (Format.formatter -> unit)]
        ; code = fun _ _ x -> x
        }
      ; migrate_exn = {
          srctype = [%typ: exn]
        ; dsttype = [%typ: exn]
        ; code = fun _ _ x -> x
        }
      ; migrate_out_name = {
          srctype = [%typ: out_name]
        ; dsttype = [%typ: string]
        ; code = fun _ _ { printed_name = printed_name } -> printed_name
        }
      ; migrate_out_type = {
          srctype = [%typ: out_type]
        ; dsttype = [%typ: DST.Outcometree.out_type]
        ; custom_branches_code = function
              Otyp_module
                (Oide_ident { printed_name = v_0 },
                 v_1,
                 v_2) ->
              let open DST.Outcometree in
              Otyp_module (v_0, v_1,
                           List.map (__dt__.migrate_out_type __dt__ __inh__) v_2)
            | Otyp_module _ -> migration_error __inh__ "Otyp_module:out_ident"
        }
      }
    }
]
