open OUnit2

open Ex_ast
open Ex_migrate

let test_ast1_ast2 ctxt =
  assert_equal AST2.({ it = C 1 ; extra = 3; new_field = 3 }) 
Migrate_AST1_AST2.(dt.migrate_t4 dt AST1.{ it = C true ; extra = 3 ; dropped_field = "1" })

let test_ast2_ast1 ctxt =
  assert_equal AST1.(A("1", [2;3])) 
    Migrate_AST2_AST1.(dt.migrate_t1 dt AST2.(A(1, [2;3])))

let suite = "test_ex" >::: [
    "test_ast1_ast2"   >:: test_ast1_ast2
  ; "test_ast2_ast1"   >:: test_ast2_ast1
  ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
