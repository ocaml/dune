Test that vendor stanza requires dune language 3.22+

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > EOF

  $ mkdir -p mylib.1.0.0
  $ cat > mylib.1.0.0/dune-project << EOF
  > (lang dune 3.21)
  > (package (name mylib))
  > EOF

  $ cat > mylib.1.0.0/dune << EOF
  > (library
  >  (name mylib)
  >  (public_name mylib))
  > EOF

  $ cat > mylib.1.0.0/mylib.ml << EOF
  > let x = 1
  > EOF

  $ cat > dune << EOF
  > (vendor mylib.1.0.0)
  > EOF

  $ dune build 2>&1 | head -5
  File "dune", line 1, characters 0-20:
  1 | (vendor mylib.1.0.0)
      ^^^^^^^^^^^^^^^^^^^^
  Error: 'vendor' is only available since version 3.22 of the dune language.
  Please update your dune-project file to have (lang dune 3.22).
  [1]
