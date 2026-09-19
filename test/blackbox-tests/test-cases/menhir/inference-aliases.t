Menhir inference can expose a shadowed ancestor alias, not just the parser's
own group alias (#8989).

  $ make_menhir_project 3.11 2.1
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

  $ dune build lib.cma 2>errors
  [1]
  $ sed -n '/val xv_main/p' _build/default/outer/inner/inner__mock.mli.inferred
  val xv_main : (module Outer__/2.Ast.S)
  $ sed -n '/Error: Syntax error/{p;q;}' errors
  Error: Syntax error
