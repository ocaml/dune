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
  >  (name unused_lib2)
  >  (modules unused_lib2))
  > 
  > (library
  >  (name mylib)
  >  (modules mylib)
  >  (libraries used_lib unused_lib unused_lib2))
  > EOF

  $ cat > used_lib.ml <<EOF
  > let helper x = x + 1
  > EOF

  $ cat > unused_lib.ml <<EOF
  > let unused_helper x = x * 2
  > EOF

  $ cat > unused_lib2.ml <<EOF
  > let unused_helper x = x * 3
  > EOF

  $ cat > mylib.ml <<EOF
  > let compute x = Used_lib.helper x
  > EOF

  $ dune build @unused-libs
  File "dune", line 16, characters 32-43:
  16 |  (libraries used_lib unused_lib unused_lib2))
                                       ^^^^^^^^^^^
  Error: Unused libraries:
  - unused_lib2
  - unused_lib
  [1]
