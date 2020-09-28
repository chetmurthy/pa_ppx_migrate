open OUnit2

open Ex_camlp5
open Ex_map_camlp5
open Pa_ppx_base
open Pa_ppx_testutils
open Papr_util

open Ex_map_camlp5

type reloc_aux = { floc : Ploc.t -> Ploc.t ; sh : int }

let anti_loc qloc sh loc loc1 =
  (*
    ...<:expr<.....$lid:...xxxxxxxx...$...>>...
    |..|-----------------------------------|    qloc
       <----->                                  sh
              |.........|------------|          loc
                        |..|------|             loc1
  *)
  let sh1 = Ploc.first_pos qloc + sh in
  let sh2 = sh1 + Ploc.first_pos loc in
  let line_nb_qloc = Ploc.line_nb qloc in
  let line_nb_loc = Ploc.line_nb loc in
  let line_nb_loc1 = Ploc.line_nb loc1 in
  if line_nb_qloc < 0 || line_nb_loc < 0 || line_nb_loc1 < 0 then
    Ploc.make_unlined
      (sh2 + Ploc.first_pos loc1, sh2 + Ploc.last_pos loc1)
  else
    Ploc.make_loc (Ploc.file_name loc)
      (line_nb_qloc + line_nb_loc + line_nb_loc1 - 2)
      (if line_nb_loc1 = 1 then
         if line_nb_loc = 1 then Ploc.bol_pos qloc
         else sh1 + Ploc.bol_pos loc
       else sh2 + Ploc.bol_pos loc1)
      (sh2 + Ploc.first_pos loc1, sh2 + Ploc.last_pos loc1) ""

let make_reloc floc sh =
  let dt =  make_dt { floc = floc ; sh = sh } in
  { (dt) with
    migrate_loc = (fun dt x -> dt.aux.floc x)
  ; migrate_patt =
      (let old_migrate_patt = dt.migrate_patt in
       (fun dt -> function
            PaAnt (loc, x1) ->
        let new_floc loc1 = anti_loc (dt.aux.floc loc) dt.aux.sh loc loc1 in
        let dt = { (dt) with aux = { (dt.aux) with floc = new_floc } } in
        dt.migrate_patt dt x1
      | x -> old_migrate_patt dt x
       ))
  ; migrate_expr =
      (let old_migrate_expr = dt.migrate_expr in
       (fun dt -> function
            ExAnt (loc, x1) ->
        let new_floc loc1 = anti_loc (dt.aux.floc loc) dt.aux.sh loc loc1 in
        let dt = { (dt) with aux = { (dt.aux) with floc = new_floc } } in
        dt.migrate_expr dt x1
      | x -> old_migrate_expr dt x
       ))
  }

let reloc_dt = make_reloc (fun _ -> Ploc.dummy) 0

let test_eq1 ctxt =
  let l = {| 1 ; 1 ; |} |> PAPR.Implem.pa1 in
  let [(si1, _); (si2, _)] = l in
  assert_bool "should not be equal" (si1 <> si2)
; assert_equal ~msg:"should be equal"
    (reloc_dt.migrate_str_item reloc_dt si1)
    (reloc_dt.migrate_str_item reloc_dt si2)
;;
Pp_MLast.Ploc.pp_loc_verbose := true ;;

let test_anti ctxt =
  let e1 = match {| 

1 ; |} |> PAPR.Implem.pa1 ~input_file:"file1" with
      [(<:str_item< $exp:e$ >>, _)] -> e in
  let loc = Ploc.make_unlined (10, 20) in
  let loc2 = Ploc.make_unlined (15, 18) in
  let e2 = <:expr< 1 + $ExAnt (loc2, e1)$ >> in
  assert_bool "should not be equal"
    (Reloc.expr (fun _ -> Ploc.dummy) 0 e2 <> e2)
; assert_bool "should not be equal"
    (Reloc.expr (fun _ -> Ploc.dummy) 100 e2 <> e2)
; assert_equal ~msg:"should be equal"
    ~printer:Pp_MLast.show_expr
    (Reloc.expr (fun _ -> Ploc.dummy) 0 e2)
    (reloc_dt.migrate_expr reloc_dt e2)  

let suite = "test_ex_camlp5" >::: [
    "test_eq1"   >:: test_eq1
  ; "test_anti"   >:: test_anti
  ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
