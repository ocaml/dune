Test interaction of melange library mode + `(stdlib ..)` field in the library
stanza

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > (using melange 1.0)
  > (using experimental_building_ocaml_compiler_with_dune 0.1)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name mystdlib)
  >  (stdlib)
  >  (modes melange)
  >  (flags :standard -w -8))
  > EOF

  $ cat > mystdlib.ml << EOF
  > (* This triggers warning 8 *)
  > let None = None
  > EOF

  $ dune build

Add a root_module that is the stdlib alias, too

  $ cat > dune << EOF
  > (library
  >  (name mystdlib)
  >  (stdlib)
  >  (root_module mystdlib)
  >  (modes melange))
  > EOF

  $ dune build
