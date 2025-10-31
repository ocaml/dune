Test that the unused-libs alias cannot detect some instances of unused
libraries

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name used_lib)
  >  (libraries unused_lib)
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
  > include Unused_lib
  > let helper x = x + 1
  > EOF

  $ cat > unused_lib.ml <<EOF
  > let unused_helper x = x * 2
  > EOF

  $ cat > mylib.ml <<EOF
  > let compute x = Used_lib.helper x
  > EOF

  $ dune build @unused-libs
