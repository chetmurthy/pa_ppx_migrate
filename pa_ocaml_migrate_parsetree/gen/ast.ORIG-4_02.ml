# 1 "ast.ORIG.ml"
(* camlp5r *)
(* pp_parsetree.ml,v *)

module Lexing = struct
[%%import: 
# 5 "ast.ORIG.ml"
             
# 5 "ast.ORIG.ml"
                  Lexing.position] 
end


# 14 "ast.ORIG.ml"
module Location = struct
[%%import: 
# 15 "ast.ORIG.ml"
             
# 15 "ast.ORIG.ml"
                  Location.t] 
[%%import: 'a 
# 16 "ast.ORIG.ml"
                
# 16 "ast.ORIG.ml"
                     Location.loc] 
end
module Longident = struct
[%%import: 
# 19 "ast.ORIG.ml"
             
# 19 "ast.ORIG.ml"
                  Longident.t] 
end
module Asttypes = struct
[%%import: 
# 22 "ast.ORIG.ml"
             
# 22 "ast.ORIG.ml"
                  Asttypes.loc] 
# 26 "ast.ORIG.ml"
[%%import: 
# 26 "ast.ORIG.ml"
             
# 26 "ast.ORIG.ml"
                  Asttypes.label] 
[%%import: 
# 27 "ast.ORIG.ml"
             
# 27 "ast.ORIG.ml"
                  Asttypes.closed_flag] 
[%%import: 
# 28 "ast.ORIG.ml"
             
# 28 "ast.ORIG.ml"
                  Asttypes.rec_flag] 
[%%import: 
# 29 "ast.ORIG.ml"
             
# 29 "ast.ORIG.ml"
                  Asttypes.direction_flag] 
[%%import: 
# 30 "ast.ORIG.ml"
             
# 30 "ast.ORIG.ml"
                  Asttypes.private_flag] 
[%%import: 
# 31 "ast.ORIG.ml"
             
# 31 "ast.ORIG.ml"
                  Asttypes.mutable_flag] 
[%%import: 
# 32 "ast.ORIG.ml"
             
# 32 "ast.ORIG.ml"
                  Asttypes.virtual_flag] 
[%%import: 
# 33 "ast.ORIG.ml"
             
# 33 "ast.ORIG.ml"
                  Asttypes.override_flag] 
[%%import: 
# 34 "ast.ORIG.ml"
             
# 34 "ast.ORIG.ml"
                  Asttypes.variance] 
# 36 "ast.ORIG.ml"
[%%import: 
# 36 "ast.ORIG.ml"
             
# 36 "ast.ORIG.ml"
                  Asttypes.constant] 
# 38 "ast.ORIG.ml"
end
module Parsetree = struct
open Asttypes
# 44 "ast.ORIG.ml"
type location_stack = Location.t list 
[%%import: 
# 45 "ast.ORIG.ml"
             
# 45 "ast.ORIG.ml"
                  Parsetree.attribute] 
end


# 54 "ast.ORIG.ml"
module Outcometree = struct
# 58 "ast.ORIG.ml"
[%%import: 
# 58 "ast.ORIG.ml"
             
# 58 "ast.ORIG.ml"
                  Outcometree.out_ident] 
# 65 "ast.ORIG.ml"
[%%import: 
# 65 "ast.ORIG.ml"
             
# 65 "ast.ORIG.ml"
                  Outcometree.out_value] 
[%%import: 
# 66 "ast.ORIG.ml"
             
# 66 "ast.ORIG.ml"
                  Outcometree.out_type] 
[%%import: 
# 67 "ast.ORIG.ml"
             
# 67 "ast.ORIG.ml"
                  Outcometree.out_class_type] 
[%%import: 
# 68 "ast.ORIG.ml"
             
# 68 "ast.ORIG.ml"
                  Outcometree.out_module_type] 
[%%import: 
# 69 "ast.ORIG.ml"
             
# 69 "ast.ORIG.ml"
                  Outcometree.out_phrase] 
end
