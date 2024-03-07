  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > (using experimental_building_ocaml_compiler_with_dune 0.1)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name mystdlib)
  >  (stdlib)
  >  (flags :standard -w -8))
  > EOF

  $ cat > mystdlib.ml << EOF
  > (* This triggers warning 8 *)
  > let None = None
  > EOF

  $ dune build
