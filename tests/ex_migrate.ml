
module Migrate_AST1_AST2 = struct

module SRC = Ex_ast.AST1
module DST = Ex_ast.AST2

exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

let _migrate_list subrw0 __dt__ l =
  List.map (subrw0 __dt__) l

type t0 = [%import: Ex_ast.AST1.t0]
and t1 = [%import: Ex_ast.AST1.t1]
and 'a pt2 = [%import: 'a Ex_ast.AST1.pt2]
and t2 = [%import: Ex_ast.AST1.t2]
and 'a pt3 = [%import: 'a Ex_ast.AST1.pt3]
and t4 = [%import: Ex_ast.AST1.t4]
and t4' = [%import: Ex_ast.AST1.t4']
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Ex_ast.AST1
        ; dstmod = Ex_ast.AST2
        ; types = [
            t1
          ; pt2
          ; t4'
          ]
        }
      ]
    ; dispatchers = {
        migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_t0 = {
          srctype = [%typ: t0]
        ; dsttype = [%typ: DST.t0]
        ; code = fun __dt__ s ->
            match int_of_string s with
              n -> n
            | exception Failure _ -> migration_error "t0"
        }
      ; migrate_t2 = {
          srctype = [%typ: t2]
        ; dsttype = [%typ: DST.t2]
        ; custom_branches_code = function
              C true -> C 1
            | C false -> C 0
            | D -> migration_error "t2:D"
        }
      ; migrate_pt3 = {
          srctype = [%typ: 'a pt3]
        ; dsttype = [%typ: 'b DST.pt3]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; skip_fields = [ dropped_field ]
        ; custom_fields_code = {
            new_field = extra
          }
        }
      ; migrate_t4 = {
          srctype = [%typ: t4]
        ; dsttype = [%typ: DST.t4]
        }
      }
    }
]
end

module Migrate_AST2_AST1 = struct

module SRC = Ex_ast.AST2
module DST = Ex_ast.AST1

exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

let _migrate_list subrw0 __dt__ l =
  List.map (subrw0 __dt__) l

type t0 = [%import: Ex_ast.AST2.t0]
and t1 = [%import: Ex_ast.AST2.t1]
and 'a pt2 = [%import: 'a Ex_ast.AST2.pt2]
and t2 = [%import: Ex_ast.AST2.t2]
and 'a pt3 = [%import: 'a Ex_ast.AST2.pt3]
and t4 = [%import: Ex_ast.AST2.t4]
and t4' = [%import: Ex_ast.AST2.t4']
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Ex_ast.AST2
        ; dstmod = Ex_ast.AST1
        ; types = [
            t1
          ; pt2
          ; t4'
          ]
        }
      ]
    ; dispatchers = {
        migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_t0 = {
          srctype = [%typ: t0]
        ; dsttype = [%typ: DST.t0]
        ; code = fun __dt__ n -> string_of_int n
        }
      ; migrate_t2 = {
          srctype = [%typ: t2]
        ; dsttype = [%typ: DST.t2]
        ; custom_branches_code = function
              C 1 -> C true
            | C 0 -> C false
            | C _ -> migration_error "t2:C"
            | E -> migration_error "t2:E"
        }
      ; migrate_pt3 = {
          srctype = [%typ: 'a pt3]
        ; dsttype = [%typ: 'b DST.pt3]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; skip_fields = [ new_field ]
        ; custom_fields_code = {
            dropped_field = string_of_int extra
          }
        }
      ; migrate_t4 = {
          srctype = [%typ: t4]
        ; dsttype = [%typ: DST.t4]
        }
      }
    }
]
end
