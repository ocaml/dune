Test that the unused-libs alias exists and can be built

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name used_lib)
  >  (modules used_lib))
  > 
  > (library
  >  (name unused_lib)
  >  (modules unused_lib))
  > 
  > (library
  >  (name mylib)
  >  (modules mylib)
  >  (libraries used_lib unused_lib))
  > EOF

  $ cat > used_lib.ml <<EOF
  > let helper x = x + 1
  > EOF

  $ cat > unused_lib.ml <<EOF
  > let unused_helper x = x * 2
  > EOF

  $ cat > mylib.ml <<EOF
  > let compute x = Used_lib.helper x
  > EOF

  $ dune build @unused-libs
  File "dune", lines 9-12, characters 0-73:
   9 | (library
  10 |  (name mylib)
  11 |  (modules mylib)
  12 |  (libraries used_lib unused_lib))
  Error: Unused libraries:
  - unused_lib
  [1]
