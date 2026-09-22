Menhir inference can expose a shadowed ancestor alias, not just the parser's
own group alias (#8989).

  $ make_menhir_project 3.25 3.0
  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library (name lib) (wrapped false))
  > EOF
  $ mkdir -p outer/inner
  $ echo 'module Inner = Inner' >outer/outer.ml
  $ echo 'module type S = sig end' >outer/ast.ml
  $ echo '(menhir (modules inner))' >outer/inner/dune
  $ cat >outer/inner/inner.mly <<'EOF'
  > %token EOF
  > %start <_> main
  > %%
  > main: EOF { (module struct end : Ast.S) }
  > EOF

  $ dune build lib.cma
  File "outer/inner/inner__mock.ml.mock", line 1:
  Error (warning 63 [erroneous-printed-signature]): The printed interface
    differs from the inferred interface. The inferred interface contained items
    which could not be printed properly due to name collisions between
    identifiers. File "_none_", line 1:
    Definition of module Outer__/2 Beware
    that this warning is purely informational and will not catch all instances
    of erroneous printed interface.
  [1]

A reference through a generated subgroup interface also needs its child CMIs.

  $ mkdir outer/sub
  $ cat >outer/sub/m.mli <<'EOF'
  > type t
  > val x : t
  > EOF
  $ echo 'type t = T let x = T' >outer/sub/m.ml
  $ cat >outer/inner/inner.mly <<'EOF'
  > %token EOF
  > %start <_> main
  > %%
  > main: EOF { Sub.M.x }
  > EOF
  $ dune build --sandbox=copy lib.cma

The dependency must also work when the subgroup has a handwritten interface.

  $ echo 'module M = M' >outer/sub/sub.ml
  $ dune build --sandbox=copy lib.cma
