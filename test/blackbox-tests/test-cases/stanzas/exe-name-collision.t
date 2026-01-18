Executables using the same name, defined in the same folder.

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name foo))
  > (executable
  >  (name foo))
  > EOF

  $ cat > foo.ml <<EOF
  > let () = ()
  > EOF

  $ dune build
  File "dune", lines 1-2, characters 0-24:
  1 | (executable
  2 |  (name foo))
  Error: Executable "foo" appears for the second time in this directory
  Already defined at dune:3
  [1]
