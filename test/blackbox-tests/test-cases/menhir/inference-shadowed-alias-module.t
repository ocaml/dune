Menhir inference must print imported types using accessible module names.
Opening the private inference alias must preserve the normal alias opens
used by the compiler's short-path printer (#16946).

  $ make_menhir_project 3.8 2.1
  $ cat >dune <<'EOF'
  > (library (name foo))
  > (menhir (modules parser))
  > EOF

A handwritten library main module makes Dune generate a guarded alias Foo__.

  $ touch foo.ml

Create a type abbreviation that Helper will mention in its interface.

  $ cat >parsetree.ml <<'EOF'
  > type result = string list
  > EOF

Helper is compiled with [-open Foo__], so its inferred interface stores
the type as [Foo__.Parsetree.result].

  $ cat >helper.ml <<'EOF'
  > let keep (x : Parsetree.result) = x
  > EOF

The parser uses [Helper.keep] and must infer a printable result type.

  $ cat >parser.mly <<'EOF'
  > %token EOF
  > %start <Parsetree.result> main
  > %%
  > main: EOF { Helper.keep [] }
  > EOF

  $ dune build foo.cma
  $ cat _build/default/parser__mock.mli.inferred
  type token = EOF
  val menhir_begin_marker : int
  val xv_main : Parsetree.result
  val menhir_end_marker : int
