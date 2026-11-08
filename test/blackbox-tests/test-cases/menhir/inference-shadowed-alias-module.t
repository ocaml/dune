https://github.com/ocaml/dune/pull/16464 changes how module opens are handled
for menhir rules. This test shows how it can lead to breakage via module
shadowing errors and identiefier lookup failures.

  $ make_menhir_project 3.8 2.1
  $ cat >dune <<'EOF'
  > (library (name foo))
  > (menhir (modules parser))
  > EOF

Create the library's module alias, to trigger dune's name-mangling module name hacks:

  $ touch foo.ml

Create a new module, 'Parsetree', that defines an abbreviation. Helper mentions it in its interface, where
the compiler records it as Foo__.Parsetree.attributes.

  $ cat >parsetree.ml <<'EOF'
  > type result = string list
  > EOF

Create a helper module that refers to the type from parsetree.ml, to trigger
dune's addition of the [-open Foo__] when it is compiled (required to grant
access to the [Foo__.Parsetree.result]).

  $ cat >helper.ml <<'EOF'
  > let keep (x : Parsetree.result) = x
  > EOF

Now define the parser that will try to use the [Helper.keep]

  $ cat >parser.mly <<'EOF'
  > %token EOF
  > %start <Parsetree.result> main
  > %%
  > main: EOF { Helper.keep [] }
  > EOF

  $ dune build foo.cma
  File "parser.mli", line 13, characters 56-61:
  13 | val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Foo__.Parsetree.result)
                                                               ^^^^^
  Error (alert deprecated): module Foo__.Foo__
  this module is shadowed
  
  File "parser.mli", line 13, characters 56-71:
  13 | val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Foo__.Parsetree.result)
                                                               ^^^^^^^^^^^^^^^
  Error: Unbound module Foo__.Parsetree
  [1]

