Menhir inference must avoid shadowed aliases from ancestor groups as well as
the parser's own group (#8989).

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
