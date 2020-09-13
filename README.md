A PPX Rewriter to help migrate from one version of an AST to another.

### Version

This is ``pa_ppx_migrate`` (alpha) version 0.01.

# Overview

This is a PPX rewriter to help write "migrations" from one version of
an AST type to another.  For example, to migrate the Ocaml 4.02 AST,
to the Ocaml 4.03 AST, or in the backward direction.  There's already
a package that does this: `ocaml-migrate-parsetree`, and this package
is intended as a demonstration that significant automated assistance
can be brought to bear on the problem.

## Tests and Examples

This project ships with a simple example of an AST migration (the one
explained below) in the directory `test` as well as a full set of
migrations for the Ocaml AST in the directory
`pa_ocaml_migrate_parsetree`.  Both are built as part of the standard
build procedure.

# How it works: A slightly long-winded explanation

If you think about it, `ppx_deriving.map` isn't so different from what
we need for `ocaml-migrate-parsetree`.  And with 11 versions of the
Ocaml AST so far, maybe it's worth thinking about how to automate more
of the task.  Also, since so much of it is type-structure-driven, one
would think that it would be an excellent opportunity to apply PPX
rewriting technology.  **Indeed, one might think that a good test of
PPX rewriting, is the ability to automate precisely such tasks.**

So what's hard about this migration task?  Here are some issues (maybe there are more):
1. the types are slightly differently-organized in different versions of the AST.  Type might move from one module to another.
2. sometimes new types are introduced and old ones disappear
3. constructor data-types may get new branches, or lose them
4. record-types may get new fields, or lose them
5. sometimes the analogous types in two consecutive versions are just really, really different [but this is rare]: we need to supply the code directly
6. when mapping from one version to another, sometimes features are simply not mappable, and an error needs to be raised; that error ought to contain an indication of where in the source that offending feature was found
7. And finally, when all else fails, we might need to hack on the migration code directly

But morally, the task is really simple (with problems listed in-line):

1. use `ppx_import` to copy over types from each of the AST times of each Ocaml version

   - `ppx_import` works on `.cmi` files, and those have different formats in different versions of Ocaml.  Wouldn't it be nice if it worked on `.mli` files, which (again b/c OCaml is well-managed) whose syntax doesn't change much?

2. build a single OCaml module that has all the AST types in it (from all the versions of OCaml)

  - but without the form
```
type t = t2 = A of .. | B of ....
```
	that is, without the "type equation" that allows for a new type-definition to precisely repeat a previous one.  

3. Then use `ppx_import` against this single module to construct a recursive type-declaration list of all the AST types for a particular version of OCaml, and apply a "souped-up" version of ppx_deriving.map to it, to map the types to *another* version of the AST types.

  -- but `ppx_deriving.map` doesn't do this today, and besides, it would have to provide a bunch of "escape hatches" for all the special-cases I mentioned above.
  
But this is in principle doable, and it has the nice feature that all the tedious boilerplate is mechanically-generated from type-definitions, hence likely to not contain errors (assuming the PPX rewriter isn't buggy).

So I decided to do it.

# A detailed explanation of how it works

## The ASTs

Suppose that I have two AST types (in `ex_ast.ml`):
```
module AST1 = struct
type t0 = string
type t1 = A of t1 * int list
type t2 = B of string * t0 | C of bool | D
type 'a pt3 = { it : 'a ; extra : int ; dropped_field: string }
type t4 = t2 pt3
end

module AST2 = struct
type t0 = int
type t1 = A of t1 * int list
type t2 = B of string * t0 | C of int | E
type 'a pt3 = { it : 'a ; extra : int ; new_field : int }
type t4 = t2 pt3
end
```
These AST types differ in the following ways:
1. `t0` is just completely different in these versions
2. `t1` is identical, but references a type that is different in each version; also an externally-defined polymorphic type-constructor (` 'a list`)
3. `t2` differs in each version: it loses a branch (`D`), and gains a branch (`E`).  It also has a branch whose arguments change type (`C`)
4. ` 'a pt3` similarly loses a field and gains a field; it's also a polymorphic type
5. `t4` is identical, but again references types that are different in each version.

## The Migrations

We'd like to write migrations that incur the -least- amount of boilerplate.  What would that mean?  For starters, we need to match up the types: there's no way to avoid that.  Next, for branches that change type, we need to provide code.  We need a way to flag branches that appear or disappear, and similarly for fields, to compute the value of fields that appear.  So we'll:

1. list the types we're going to work with, importing their definitions, so that the migration code will work against those already-defined types
2. specify pairs of source-type, destination-type patterns, and for each, perhaps supply the function, or code for new/disappearing fields/branches
3. for polymorphic types, we need to specify how their type-parameters get rewritten.

Below, we'll describe the process for taking this information and automatically computing the code of these migrations.

```

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
and t2 = [%import: Ex_ast.AST1.t2]
and 'a pt3 = [%import: 'a Ex_ast.AST1.pt3]
and t4 = [%import: Ex_ast.AST1.t4]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_value = dt
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
      ; migrate_t1 = {
          srctype = [%typ: t1]
        ; dsttype = [%typ: DST.t1]
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
and t2 = [%import: Ex_ast.AST2.t2]
and 'a pt3 = [%import: 'a Ex_ast.AST2.pt3]
and t4 = [%import: Ex_ast.AST2.t4]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_value = dt
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
      ; migrate_t1 = {
          srctype = [%typ: t1]
        ; dsttype = [%typ: DST.t1]
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
```

## Using the Migrations

And we can use them in the toplevel to migrate in each direction:

```
#load "ex_ast.cmo";;
#load "ex_migrate.cmo";;
open Ex_migrate ;;
open Ex_ast ;;
# Migrate_AST1_AST2.(dt.migrate_t4 dt AST1.{ it = C true ; extra = 3 ; dropped_field = "1" });;
- : Ex_migrate.Migrate_AST1_AST2.DST.t4 =
{Ex_migrate.Migrate_AST1_AST2.DST.it = Ex_migrate.Migrate_AST1_AST2.DST.C 1;
 extra = 3; new_field = 3}
# Migrate_AST2_AST1.(dt.migrate_t1 dt AST2.(A(1, [2;3])));;
- : Ex_migrate.Migrate_AST2_AST1.DST.t1 =
Ex_migrate.Migrate_AST2_AST1.DST.A ("1", [2; 3])
```

## How Do We Compute The Code?

The migration code is computed in a pretty straightforward manner.  In the following, assume we're migrating from `AST1` to `AST2` (`DST` is also the same as `AST2`).  Let's call a "migration function" something of type
```
type ('a, 'b) migrater_t = dispatch_table_t -> 'a -> 'b
```
where `dispatch_table_t` will be defined later.  First, assume (recursively) that each each migration rule yields a
migration function.  So a rule like

```
      ; migrate_t1 = {
          srctype = [%typ: t1]
        ; dsttype = [%typ: DST.t1]
        }

```
will yield a function of type
```
(AST1.t1, AST2.t1) migrater_t
```
and a rule like
```
      ; migrate_pt3 = {
          srctype = [%typ: 'a pt3]
        ; dsttype = [%typ: 'b DST.pt3]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; skip_fields = [ dropped_field ]
        ; custom_fields_code = {
            new_field = extra
          }
        }
```
will yield
```
'a 'b. ('a, 'b) migrater_t -> ('a AST1.pt3, 'b AST2.pt3) migrater_t
```
Notice that this is a function that takes a migration from ` 'a` to ` 'b`, and produces one from ` 'a AST1.pt3` to ` 'a AST2.pt3`.  So from a source-type, we need to mechanically compute the code, and also compute the result-type.  This will be key idea: the migration process, applied to a source-type, will produce both the code that migrates that source-type, and *also* the destination-type.

Now, consider any rewrite rule, and let's argue by cases on how its code & destination-type can be computed.

1. suppose that the *source type* (the field `srctype`) of the rule can be *head-reduced* (by applying some type-definition as an abbreviation) to an algebraic sum-type (`A of ... | B of ...`) or record-type (`{a: t1 ; b : ... }`).  Then we can generate code to pattern-match on values of the source-type, and for variables in the generated patterns, we know their types.  We can then apply (via pattern-matching over types) *all* the rewrite-rules to compute code that will migrate the variables' values.  Since the recursive application of migration-rules produce destination-types, we can substitute those into branches/fields, to get the destination-type.  [More on pattern-matching below.]

	- some of the branches of a sum-type might have their code specified in a `custom_branches_code`, and those branches supersede what would have been automatically-generated.
	
	- some of the fields of a record-type are listed in `skip_fields`, and they're not generated; some of the fields are listed with code to compute them (based on the fields in the pattern above) in `custom_fields_code`, and those get added.

2. suppose that the *source type* after a *single* head-reduction yields a type-expression that can be successfully pattern-matched by one of the rewrite-rules *other than this rule we're currently processing*; then we can use that rewriter function to migrate the value of the source-type, and again, we get the destination-type.

   - Why other than the current rule?  It's simple: we could end up in an infinite recursion if care isn't taken to write the migration rules correctly.

3. The process of pattern-matching takes as input a type-expression, and a migration-rule.  If the source-type of the migration-rule matches the type-expression (in the usual sense), then it produces bindings for the type-variables.

Let's look at an example.  Suppose we want to compute the migration function for the type-expression `t4`.

1. head-reducing once yields `t2 pt3`.
2. the rule `migrate_pt3` matches (source-type `'a pt3`), with type-variable ` 'a` bound to `t2`.

   - also, (via the `subs` field) if the type bound to ` 'a` is rewritten to ` 'b` then the destination-type is ` 'b AST2.pt3` [remember this below]

3. the type-expression `t2` matches the rule `migrate_t2`, with no type-variables bound and destination-type `AST2.t2`.
4. So `migrate_t2 : (AST1.t2, AST2.t2) migrater_t` (that is, the destination type is `AST2.t2`)
5. Thus in #2 above, ` 'b` is bound to `AST2.t2`, and hence the destination-type in #2 above is `AST2.t2 AST2.pt3`

So the whole migration code for type `t4` is `migrate_pt3 migrate_t2` (which is what we would expect).

## The "Dispatch Table"

Throughout this, I haven't explained what the "dispatch table" is for.
In everything above, we could have generated all the migration
functions in a big let-rec.  But that would force us to assume that
the generated code is already correct.  What if the generated code is
wrong, and we need to fix it somehow?  Should we edit the generated
code?  If so, how maintainable is that?  What if we need a special
version of the migration from `AST1` to `AST2`, that does something
slightly different than the standard one?

The dispatch table is a way of solving this problem.  Suppose that
(for whatever reason) we need to modify the code of one of the
migration functions.  The dispatch table has type
```
type nonrec dispatch_table_t = {
  migrate_list :
    'a 'b.
      ('a, 'b) migrater_t ->
      ('a list, 'b list) migrater_t;
  migrate_t0 : (AST1.t0, AST2.t0) migrater_t;
  migrate_t1 : (AST1.t1, AST2.t1) migrater_t;
  migrate_t2 : (AST1.t2, AST2.t2) migrater_t;
  migrate_pt3 :
    'a 'b.
      ('a, 'b) migrater_t ->
      ('a AST1.pt3, 'b AST2.pt3)
      migrater_t;
  migrate_t4 : (AST1.t4, DST.t4) migrater_t;
}
```
If we need to modify (say) the code of `migrate_t2`, we can do so with the code
```
{ dt with migrate_t2 = .... code using dt.migrate_t2 as a fallback .... }
```
and because *all* recursive calls go thru the dispatch table, this new version of `migrate_t2` will be used everywhere.


# Discussion

I think this is a quite viable approach to writing `ocaml-migrate-parsetree`, and I would encourage the PPX community to consider it.  One of the nice things about this approach, is that it relies *heavily* on PPX rewriting itself, to get the job done.  I think one of the important things we've learned in programming languages research, is that our tools need to be largely sufficient to allow us to comfortably implement those same tools.  It's a good test of the PPX infrastructure, to see if you can take tedious tasks and automate them away.
