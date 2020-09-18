
module SRC = All_ast.Ast_4_05
module DST = All_ast.Ast_4_06

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

type lexing_position = [%import: All_ast.Ast_4_05.Lexing.position]
and location_t = [%import: All_ast.Ast_4_05.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_05.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_05.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_05.Asttypes.label]
and arg_label = [%import: All_ast.Ast_4_05.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_4_05.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_05.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_05.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_05.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_05.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_05.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_05.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_05.Asttypes.variance]
and constant =  [%import: All_ast.Ast_4_05.Parsetree.constant]
and location_stack = [%import: All_ast.Ast_4_05.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_05.Parsetree.attribute
    [@with Asttypes.loc := location_loc]
]
and extension = [%import: All_ast.Ast_4_05.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_05.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_05.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_05.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_05.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_05.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_05.Parsetree.row_field
    [@with
      Asttypes.label := label
    ]
]
and pattern = [%import: All_ast.Ast_4_05.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_05.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_05.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_05.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_4_05.Parsetree.case]
and value_description = [%import: All_ast.Ast_4_05.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_05.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_05.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_05.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_05.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_4_05.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_4_05.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_05.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_05.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_05.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_05.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
    ]
]
and class_signature = [%import: All_ast.Ast_4_05.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_05.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_05.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ; Asttypes.loc := location_loc
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_05.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_05.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_05.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_05.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_05.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
    ]
]
and class_structure = [%import: All_ast.Ast_4_05.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_05.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_05.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_05.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_05.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_05.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_05.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and signature = [%import: All_ast.Ast_4_05.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_05.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_05.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_4_05.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_05.Parsetree.module_type_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and open_description = [%import: All_ast.Ast_4_05.Parsetree.open_description
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.override_flag := override_flag
    ]
]
and 'a include_infos = [%import: 'a All_ast.Ast_4_05.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_05.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_05.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_05.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_05.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_05.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_05.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_05.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_05.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_05.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_05.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and out_ident = [%import: All_ast.Ast_4_05.Outcometree.out_ident]
and out_attribute = [%import: All_ast.Ast_4_05.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_4_05.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_05.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_05.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_05.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_05.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_05.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_05.Outcometree.out_sig_item]
and out_type_decl = [%import: All_ast.Ast_4_05.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag]
]
and out_extension_constructor = [%import: All_ast.Ast_4_05.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_05.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_4_05.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_4_05.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_05.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_05.Outcometree.out_phrase]


[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_value = dt
    ; default_dispatchers = [
        {
          srcmod = All_ast.Ast_4_05
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = All_ast.Ast_4_05.Asttypes
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
        srcmod = All_ast.Ast_4_05.Parsetree
      ; dstmod = DST.Parsetree
      ; types = [
          attribute
        ; attributes
        ; case
        ; class_declaration
        ; class_description
        ; class_expr
        ; class_expr_desc
        ; class_field
        ; class_field_desc
        ; class_field_kind
        ; class_infos
        ; class_signature
        ; class_structure
        ; class_type
        ; class_type_declaration
        ; class_type_desc
        ; class_type_field
        ; class_type_field_desc
        ; constant
        ; constructor_arguments
        ; constructor_declaration
        ; core_type
        ; expression
        ; expression_desc
        ; extension
        ; extension_constructor
        ; extension_constructor_kind
        ; include_declaration
        ; include_description
        ; include_infos
        ; label_declaration
        ; location_stack
        ; module_binding
        ; module_declaration
        ; module_expr
        ; module_expr_desc
        ; module_type
        ; module_type_declaration
        ; module_type_desc
        ; open_description
        ; package_type
        ; pattern
        ; pattern_desc
        ; payload
        ; signature
        ; signature_item
        ; signature_item_desc
        ; structure
        ; structure_item
        ; structure_item_desc
        ; type_declaration
        ; type_extension
        ; type_kind
        ; value_binding
        ; value_description
        ]
      ; inherit_code = {
          class_expr = Some pcl_loc
        ; class_field = Some pcf_loc
        ; class_infos = Some pci_loc
        ; class_type_field = Some pctf_loc
        ; class_type = Some pcty_loc
        ; constructor_declaration = Some pcd_loc
        ; core_type = Some ptyp_loc
        ; expression = Some pexp_loc
        ; extension_constructor = Some pext_loc
        ; include_infos = Some pincl_loc
        ; label_declaration = Some pld_loc
        ; module_binding = Some pmb_loc
        ; module_declaration = Some pmd_loc
        ; module_expr = Some pmod_loc
        ; module_type_declaration = Some pmtd_loc
        ; module_type = Some pmty_loc
        ; open_description = Some popen_loc
        ; pattern = Some ppat_loc
        ; signature_item = Some psig_loc
        ; structure_item = Some pstr_loc
        ; type_declaration = Some ptype_loc
        ; value_binding = Some pvb_loc
        ; value_description = Some pval_loc
        }
      }
      ; {
        srcmod = All_ast.Ast_4_05.Outcometree
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
        ; out_type
        ; out_type_decl
        ; out_type_extension
        ; out_val_decl
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
      ; migrate_core_type_desc = {
          srctype = [%typ: core_type_desc]
        ; dsttype = [%typ: DST.Parsetree.core_type_desc]
        ; custom_branches_code = function
            | Ptyp_object (v_0, v_1) ->
              let open DST.Parsetree in
              Ptyp_object
                (List.map (fun (v_0, v_1, v_2) ->
                     Otag(__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ v_0,
                          __dt__.migrate_attributes __dt__ __inh__ v_1,
                          __dt__.migrate_core_type __dt__ __inh__ v_2)) v_0,
                 __dt__.migrate_closed_flag __dt__ __inh__ v_1)
        }
      ; migrate_row_field = {
          srctype = [%typ: row_field]
        ; dsttype = [%typ: DST.Parsetree.row_field]
        ; custom_branches_code = function
              Rtag (v_0, v_1, v_2, v_3) ->
              let open DST.Parsetree in
              Rtag
                (__dt__.migrate_location_loc __dt__.migrate_label __dt__ __inh__ (wrap_loc __inh__ v_0),
                 __dt__.migrate_attributes __dt__ __inh__ v_1,
                 v_2,
                 List.map (__dt__.migrate_core_type __dt__ __inh__) v_3)
        }
      ; migrate_with_constraint = {
          srctype = [%typ: with_constraint]
        ; dsttype = [%typ: DST.Parsetree.with_constraint]
        ; custom_branches_code = function
            | Pwith_typesubst x0 ->
              let lid_loc = map_loc (fun x -> Lident x) x0.ptype_name in 
              let open DST.Parsetree in
              Pwith_typesubst
                (__dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ lid_loc,
                 __dt__.migrate_type_declaration __dt__ __inh__ x0)

            | Pwith_modsubst (v_0, v_1) ->
              let lid_loc = map_loc (fun x -> Lident x) v_0 in 
              let open DST.Parsetree in
              Pwith_modsubst
                (__dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ lid_loc,
                 __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ v_1)

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
      ; migrate_out_value = {
          srctype = [%typ: out_value]
        ; dsttype = [%typ: DST.Outcometree.out_value]
        ; custom_branches_code = function
            | Oval_string v_0 ->
              let open DST.Outcometree in
              Oval_string (v_0, max_int, Ostr_string)
        }
      }
    }
]
