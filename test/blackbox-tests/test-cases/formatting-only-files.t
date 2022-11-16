  $ cat > dune-project << EOF
  > (lang dune 3.5)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name l))
  > 
  > (env
  >  (_
  >   (formatting (files :standard \ a.ml))))
  > EOF

  $ touch .ocamlformat

  $ cat > a.ml << EOF
  > let x      =1
  > EOF

  $ cp a.ml b.ml

  $ dune build @fmt
  File "a.ml", line 1, characters 0-0:
  Error: Files _build/default/a.ml and _build/default/.formatted/a.ml differ.
  File "b.ml", line 1, characters 0-0:
  Error: Files _build/default/b.ml and _build/default/.formatted/b.ml differ.
  [1]
'