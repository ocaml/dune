Demonstrate unused-libs issues with `(modes melange)`

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (using melange 1.0)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name used_lib)
  >  (modes melange)
  >  (modules used_lib))
  > 
  > (library
  >  (name unused_lib)
  >  (modes melange)
  >  (modules unused_lib))
  > 
  > (library
  >  (name unused_lib2)
  >  (modes melange)
  >  (modules unused_lib2))
  > 
  > (library
  >  (name mylib)
  >  (modules mylib)
  >  (modes melange)
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
  Error: No rule found for .mylib.objs/byte/mylib.cmi
  -> required by alias unused-libs in dune:16
  Error: No rule found for .mylib.objs/byte/mylib.cmo
  -> required by alias unused-libs in dune:16
  Error: No rule found for .unused_lib.objs/byte/unused_lib.cmi
  -> required by alias unused-libs in dune:6
  Error: No rule found for .unused_lib.objs/byte/unused_lib.cmo
  -> required by alias unused-libs in dune:6
  Error: No rule found for .unused_lib2.objs/byte/unused_lib2.cmi
  -> required by alias unused-libs in dune:11
  Error: No rule found for .unused_lib2.objs/byte/unused_lib2.cmo
  -> required by alias unused-libs in dune:11
  Error: No rule found for .used_lib.objs/byte/used_lib.cmi
  -> required by alias unused-libs in dune:1
  Error: No rule found for .used_lib.objs/byte/used_lib.cmo
  -> required by alias unused-libs in dune:1
  [1]

