open OUnit2

open Ex_camlp5
open Ex_map_camlp5
open Pa_ppx_testutils
open Papr_util

let reloc_dt =
  let dt =  make_dt (fun _ -> Ploc.dummy) in
  { (dt) with migrate_loc = (fun _ x -> dt.aux x) }

let test_eq1 ctxt =
  let l = {| 1 ;; 1 |} |> PAPR.Implem.pa1 in
  let [(si1, _); (si2, _)] = l in
  assert_bool "should be not equal" (si1 <> si2)
; assert_equal ~msg:"should be equal"
    (reloc_dt.migrate_str_item reloc_dt si1)
    (reloc_dt.migrate_str_item reloc_dt si2)

let suite = "test_ex_camlp5" >::: [
    "test_eq1"   >:: test_eq1
  ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
