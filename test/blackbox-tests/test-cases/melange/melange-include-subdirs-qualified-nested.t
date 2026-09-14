Test Melange compilation for a `(include_subdirs qualified)` stanza appearing
in a nested subdirectory.

  $ make_melange_project 3.22 1.0

  $ mkdir -p a/b/c

  $ cat > a/dune <<EOF
  > (include_subdirs qualified)
  > (library (name foo) (modes melange))
  > EOF

  $ cat > a/b/c/dune <<EOF
  > (ocamllex lexer)
  > EOF
  $ make_trivial_ocamllex a/b/c/lexer.mll

  $ cat > a/foo.ml <<EOF
  > module L = B.C.Lexer
  > EOF


  $ dune build
  $ find _build/default/a/.melange_src | sort
  _build/default/a/.melange_src
  _build/default/a/.melange_src/b
  _build/default/a/.melange_src/b/c
  _build/default/a/.melange_src/b/c/lexer.ml
  _build/default/a/.melange_src/foo.ml
  _build/default/a/.melange_src/foo__.ml-gen
  _build/default/a/.melange_src/foo__B.ml-gen
  _build/default/a/.melange_src/foo__B__C.ml-gen

A generated group interface can be paired with a handwritten interface under
its original directory name.

  $ make_melange_project 3.25 1.0
  $ mkdir a/internal
  $ cat >a/dune <<EOF
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (internal as public)))
  > (library (name foo) (modes melange))
  > EOF
  $ cat >a/internal/dune <<EOF
  > (ocamllex public)
  > EOF
  $ cat >a/internal/public.mll <<EOF
  > rule token = parse
  > | eof { () }
  > EOF
  $ cat >a/internal/internal.mli <<EOF
  > val token : Lexing.lexbuf -> unit
  > EOF
  $ cat >a/foo.ml <<EOF
  > module L = B.C.Lexer
  > let token = Public.token
  > EOF
  $ dune build

A generated implementation must not replace a handwritten one after renaming.

  $ cat >a/internal/internal.ml <<EOF
  > let token _ = ()
  > EOF
  $ dune build
  Error: Too many files for module Public in a/internal:
  - _build/default/a/.melange_src/internal/internal.ml
  - _build/default/a/.melange_src/internal/public.ml
  -> required by alias default
  [1]
