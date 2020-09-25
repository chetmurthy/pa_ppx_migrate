
module SRC = All_ast.Ast_4_02
module DST = All_ast.Ast_4_03

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

let _migrate_list subrw0 __dt__ __inh__ l =
  List.map (subrw0 __dt__ __inh__) l

let migrate_label_arg_label : 'a -> 'b -> SRC.Asttypes.label -> DST.Asttypes.arg_label =
  fun __dt__ __inh__ x ->
    if x <> "" then
      if x.[0] = '?' then DST.Asttypes.Optional (String.sub x 1 (String.length x - 1))
      else DST.Asttypes.Labelled x
    else
      DST.Asttypes.Nolabel

let migrate_Asttypes_constant_Parsetree_constant :
  'a -> 'b -> SRC.Asttypes.constant -> DST.Parsetree.constant =
  fun __dt__ __inh__ -> function
  | SRC.Asttypes.Const_int x0 ->
      DST.Parsetree.Pconst_integer (string_of_int x0, None)
  | SRC.Asttypes.Const_char x0 ->
      DST.Parsetree.Pconst_char x0
  | SRC.Asttypes.Const_string (x0,x1) ->
      DST.Parsetree.Pconst_string
        (x0, x1)
  | SRC.Asttypes.Const_float x0 ->
      DST.Parsetree.Pconst_float (x0, None)
  | SRC.Asttypes.Const_int32 x0 ->
      DST.Parsetree.Pconst_integer (Int32.to_string x0, Some 'l')
  | SRC.Asttypes.Const_int64 x0 ->
      DST.Parsetree.Pconst_integer (Int64.to_string x0, Some 'L')
  | SRC.Asttypes.Const_nativeint x0 ->
      DST.Parsetree.Pconst_integer (Nativeint.to_string x0, Some 'n')

type lexing_position = [%import: All_ast.Ast_4_02.Lexing.position]
and location_t = [%import: All_ast.Ast_4_02.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_02.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_02.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_02.Asttypes.label
]

and closed_flag =  [%import: All_ast.Ast_4_02.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_02.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_02.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_02.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_02.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_02.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_02.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_02.Asttypes.variance]
and constant =  [%import: All_ast.Ast_4_02.Asttypes.constant]
and location_stack = [%import: All_ast.Ast_4_02.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_02.Parsetree.attribute
    [@with Asttypes.loc := location_loc]
]
and extension = [%import: All_ast.Ast_4_02.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_02.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_02.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_02.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_02.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_02.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_02.Parsetree.row_field
    [@with
      Asttypes.label := label
    ]
]
and pattern = [%import: All_ast.Ast_4_02.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_02.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.constant := constant ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_02.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_02.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
      Asttypes.constant := constant
    ]
]
and case = [%import: All_ast.Ast_4_02.Parsetree.case]
and value_description = [%import: All_ast.Ast_4_02.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_02.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_02.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_02.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_02.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_extension = [%import: All_ast.Ast_4_02.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_02.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_02.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_02.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_02.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
    ]
]
and class_signature = [%import: All_ast.Ast_4_02.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_02.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_02.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_02.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_02.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_02.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_02.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_02.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
    ]
]
and class_structure = [%import: All_ast.Ast_4_02.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_02.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_02.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_02.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_02.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_02.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_02.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and signature = [%import: All_ast.Ast_4_02.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_02.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_02.Parsetree.signature_item_desc]
and module_declaration = [%import: All_ast.Ast_4_02.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_02.Parsetree.module_type_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and open_description = [%import: All_ast.Ast_4_02.Parsetree.open_description
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.override_flag := override_flag
    ]
]
and 'a include_infos = [%import: 'a All_ast.Ast_4_02.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_02.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_02.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_02.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_02.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_02.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_02.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_02.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_02.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_02.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_02.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and out_ident = [%import: All_ast.Ast_4_02.Outcometree.out_ident]
and out_value = [%import: All_ast.Ast_4_02.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_02.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_02.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_02.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_02.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_02.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_02.Outcometree.out_sig_item]
and out_type_decl = [%import: All_ast.Ast_4_02.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag]
]
and out_extension_constructor = [%import: All_ast.Ast_4_02.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_02.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_rec_status = [%import: All_ast.Ast_4_02.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_02.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_02.Outcometree.out_phrase]

[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = All_ast.Ast_4_02
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = All_ast.Ast_4_02.Asttypes
      ; dstmod = DST.Asttypes
      ; types = [
          closed_flag
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
        srcmod = All_ast.Ast_4_02.Parsetree
      ; dstmod = DST.Parsetree
      ; types = [
          attribute
        ; attributes
        ; case
        ; class_declaration
        ; class_description
        ; class_expr
        ; class_field
        ; class_field_desc
        ; class_field_kind
        ; class_infos
        ; class_signature
        ; class_structure
        ; class_type
        ; class_type_declaration
        ; class_type_field
        ; class_type_field_desc
        ; core_type
        ; expression
        ; extension
        ; extension_constructor
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
        ; row_field
        ; signature
        ; signature_item
        ; structure
        ; structure_item
        ; type_declaration
        ; type_extension
        ; type_kind
        ; value_binding
        ; value_description
        ; with_constraint
        ]
      ; inherit_code = {
          class_expr = Some pcl_loc
        ; class_field = Some pcf_loc
        ; class_infos = Some pci_loc
        ; class_type_field = Some pctf_loc
        ; class_type = Some pcty_loc
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
        srcmod = All_ast.Ast_4_02.Outcometree
      ; dstmod = DST.Outcometree
      ; types = [
          out_class_sig_item
        ; out_class_type
        ; out_extension_constructor
        ; out_ext_status
        ; out_ident
        ; out_module_type
        ; out_phrase
        ; out_rec_status
        ; out_type
        ; out_type_extension
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
      ; migrate_constant = {
          srctype = [%typ: constant]
        ; dsttype = [%typ: DST.Parsetree.constant]
        ; code = migrate_Asttypes_constant_Parsetree_constant
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
            Ptyp_arrow (v_0, v_1, v_2) ->
            let open DST.Parsetree in
            Ptyp_arrow
              (migrate_label_arg_label __dt__ __inh__ v_0,
               __dt__.migrate_core_type __dt__ __inh__ v_1,
               __dt__.migrate_core_type __dt__ __inh__ v_2)
        }
      ; migrate_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.Parsetree.expression_desc]
        ; custom_branches_code = function
              Pexp_fun (v_0, v_1, v_2, v_3) ->
              let open DST.Parsetree in
              Pexp_fun
                (migrate_label_arg_label __dt__ __inh__ v_0,
                 __dt__.migrate_option __dt__.migrate_expression __dt__ __inh__ v_1,
                 __dt__.migrate_pattern __dt__ __inh__ v_2,
                 __dt__.migrate_expression __dt__ __inh__ v_3)
            | Pexp_apply (v_0, v_1) ->
              let open DST.Parsetree in
              Pexp_apply
                (__dt__.migrate_expression __dt__ __inh__ v_0,
                 List.map (fun (v_0, v_1) ->
                     migrate_label_arg_label __dt__ __inh__ v_0,
                     __dt__.migrate_expression __dt__ __inh__ v_1) v_1)
        }
      ; migrate_constructor_declaration = {
          srctype = [%typ: constructor_declaration]
        ; dsttype = [%typ: DST.Parsetree.constructor_declaration]
        ; inherit_code = Some pcd_loc
        ; skip_fields = [ pcd_args ]
        ; custom_fields_code = {
            pcd_args =
              DST.Parsetree.Pcstr_tuple (List.map (__dt__.migrate_core_type __dt__ __inh__) pcd_args)
          }
        }
      ; migrate_extension_constructor_kind = {
          srctype = [%typ: extension_constructor_kind]
        ; dsttype = [%typ: DST.Parsetree.extension_constructor_kind]
        ; custom_branches_code = function
    Pext_decl (v_0, v_1) ->
      let open DST.Parsetree in
      Pext_decl
        (DST.Parsetree.Pcstr_tuple (List.map (__dt__.migrate_core_type __dt__ __inh__) v_0),
         Option.map (__dt__.migrate_core_type __dt__ __inh__) v_1)
        }
      ; migrate_class_type_desc = {
          srctype = [%typ: class_type_desc]
        ; dsttype = [%typ: DST.Parsetree.class_type_desc]
        ; custom_branches_code = function
              Pcty_arrow (v_0, v_1, v_2) ->
              let open DST.Parsetree in
              Pcty_arrow
                (migrate_label_arg_label __dt__ __inh__ v_0,
                 __dt__.migrate_core_type __dt__ __inh__ v_1,
                 __dt__.migrate_class_type __dt__ __inh__ v_2)
        }
      ; migrate_class_expr_desc = {
          srctype = [%typ: class_expr_desc]
        ; dsttype = [%typ: DST.Parsetree.class_expr_desc]
        ; custom_branches_code = function
              Pcl_fun (v_0, v_1, v_2, v_3) ->
              let open DST.Parsetree in
              Pcl_fun
                (migrate_label_arg_label __dt__ __inh__ v_0,
                 Option.map (__dt__.migrate_expression __dt__ __inh__)  v_1,
                 __dt__.migrate_pattern __dt__ __inh__ v_2,
                 __dt__.migrate_class_expr __dt__ __inh__ v_3)
            | Pcl_apply (v_0, v_1) ->
              let open DST.Parsetree in
              Pcl_apply
                (__dt__.migrate_class_expr __dt__ __inh__ v_0,
                 List.map (fun (v_0, v_1) ->
                     migrate_label_arg_label __dt__ __inh__ v_0,
                     __dt__.migrate_expression __dt__ __inh__ v_1)
                   v_1)
        }
      ; migrate_signature_item_desc = {
          srctype = [%typ: signature_item_desc]
        ; dsttype = [%typ: DST.Parsetree.signature_item_desc]
        ; custom_branches_code = function
              Psig_type v_0 ->
              let is_nonrec (attr,_) = attr.txt = "nonrec" in
              let rf = if (List.exists (fun td ->
                  List.exists is_nonrec td.ptype_attributes) v_0) then
                  DST.Asttypes.Nonrecursive
                else DST.Asttypes.Recursive in
              let open DST.Parsetree in
              Psig_type
                (rf, List.map (__dt__.migrate_type_declaration __dt__ __inh__) v_0)
        }
      ; migrate_structure_item_desc = {
          srctype = [%typ: structure_item_desc]
        ; dsttype = [%typ: DST.Parsetree.structure_item_desc]
        ; custom_branches_code = function
              Pstr_type v_0 ->
              let is_nonrec (attr,_) = attr.txt = "nonrec" in
              let rf = if (List.exists (fun td ->
                  List.exists is_nonrec td.ptype_attributes) v_0) then
                  DST.Asttypes.Nonrecursive
                else DST.Asttypes.Recursive in
              let open DST.Parsetree in
              Pstr_type
                (rf, List.map (__dt__.migrate_type_declaration __dt__ __inh__) v_0)
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

      ; migrate_out_sig_item = {
          srctype = [%typ: out_sig_item]
        ; dsttype = [%typ: DST.Outcometree.out_sig_item]
        ; custom_branches_code = function
              Osig_value (v_0, v_1, v_2) ->
              let open DST.Outcometree in
              Osig_value
                {oval_name = v_0
                ; oval_type = __dt__.migrate_out_type __dt__ __inh__ v_1
                ; oval_prims = v_2
                ; oval_attributes = []}
        }
      ; migrate_out_type_decl = {
          srctype = [%typ: out_type_decl]
        ; dsttype = [%typ: DST.Outcometree.out_type_decl]
        ; custom_fields_code = {
            otype_immediate = false
          }
        }
      }
    }
]
