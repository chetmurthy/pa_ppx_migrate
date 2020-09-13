module AST1 = struct
type t0 = string
type t1 = A of t0 * int list
type t2 = B of string * t0 | C of bool | D
type 'a pt3 = { it : 'a ; extra : int ; dropped_field: string }
type t4 = t2 pt3
end

module AST2 = struct
type t0 = int
type t1 = A of t0 * int list
type t2 = B of string * t0 | C of int | E
type 'a pt3 = { it : 'a ; extra : int ; new_field : int }
type t4 = t2 pt3
end
