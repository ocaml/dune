
  $ touch .ocamlformat
  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > EOF
  $ mkdir bin
  $ cat > bin/ocaml_file.ml << EOF
  > let  y=()
  > EOF
  $ cat > dune << EOF
  > (include_subdirs unqualified)
  > (library (name lib_reason))
  > EOF
  $ dune build ./bin/.formatted/ocaml_file.ml

.formatted dir is loaded

  $ ls _build/default/bin/.formatted
  ocaml_file.ml
