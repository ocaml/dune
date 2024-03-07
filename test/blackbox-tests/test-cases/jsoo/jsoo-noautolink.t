We check that .bc-for-jsoo targets are built with -noautolink and do not depend
on the shared stubs of dependent libraries.

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name libA)
  >  (modules libA)
  >  (foreign_stubs
  >   (language c)
  >   (names stubsA)))
  > (executable
  >  (name mainA)
  >  (libraries libA)
  >  (modules mainA)
  >  (modes js))
  > EOF
  $ touch stubsA.c mainA.ml libA.ml

  $ dune build --display verbose --profile release mainA.bc.js 2>&1 | sed -n 's#.*\(-o mainA.bc-for-jsoo .*\)#\1#p'
  -o mainA.bc-for-jsoo -no-check-prims -noautolink libA.cma .mainA.eobjs/byte/dune__exe__MainA.cmo)

The file dlllibA_stubs.so should not appear in the next list.

  $ find _build/default | sort
  _build/default
  _build/default/.dune
  _build/default/.dune/configurator
  _build/default/.dune/configurator.v2
  _build/default/.libA.objs
  _build/default/.libA.objs/byte
  _build/default/.libA.objs/byte/libA.cmi
  _build/default/.libA.objs/byte/libA.cmo
  _build/default/.libA.objs/byte/libA.cmt
  _build/default/.mainA.eobjs
  _build/default/.mainA.eobjs/byte
  _build/default/.mainA.eobjs/byte/dune__exe__MainA.cmi
  _build/default/.mainA.eobjs/byte/dune__exe__MainA.cmo
  _build/default/.mainA.eobjs/byte/dune__exe__MainA.cmt
  _build/default/.mainA.eobjs/byte/dune__exe__MainA.cmti
  _build/default/.merlin-conf
  _build/default/.merlin-conf/exe-mainA
  _build/default/.merlin-conf/lib-libA
  _build/default/libA.cma
  _build/default/libA.ml
  _build/default/mainA.bc-for-jsoo
  _build/default/mainA.bc.js
  _build/default/mainA.ml
  _build/default/mainA.mli
