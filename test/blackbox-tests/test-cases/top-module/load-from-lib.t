We try to load a module defined in a library with a dependnecy

  $ cat >dune-project <<EOF
  > (lang dune 3.3)
  > EOF

  $ mkdir foo
  $ cd foo

  $ cat >bar.ml <<EOF
  > let v = 42
  > EOF

  $ cat >foo.ml <<EOF
  > let foo = Bar.v + 42
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (libraries mydummylib)
  >  (name foo))
  > EOF

  $ cd ..

  $ mkdir mydummylib
  $ cd mydummylib
  $ cat >dune <<EOF
  > (library (name mydummylib))
  > EOF
  $ touch mydummylib.ml
  $ touch blabla.ml

  $ cd ..

  $ dune ocaml top-module foo/foo.ml
  #directory "$TESTCASE_ROOT/_build/.top/Foo.c1f1fc8a0f56";;
  #directory "$TESTCASE_ROOT/_build/default/mydummylib/.mydummylib.objs/byte";;
  #load "$TESTCASE_ROOT/_build/default/mydummylib/mydummylib.cma";;
  #load "$TESTCASE_ROOT/_build/default/foo/.foo.objs/byte/foo__Bar.cmo";;
  #load "$TESTCASE_ROOT/_build/default/foo/.foo.objs/byte/foo__.cmo";;
  #use "$TESTCASE_ROOT/_build/default/foo/foo.ml";;

  $ ls _build/.top/Foo.c1f1fc8a0f56/*.cmi
  _build/.top/Foo.c1f1fc8a0f56/foo__.cmi
  _build/.top/Foo.c1f1fc8a0f56/foo__Bar.cmi

  $ ls _build/default/mydummylib/.mydummylib.objs/byte/*.cmi
  _build/default/mydummylib/.mydummylib.objs/byte/mydummylib.cmi
  _build/default/mydummylib/.mydummylib.objs/byte/mydummylib__.cmi
  _build/default/mydummylib/.mydummylib.objs/byte/mydummylib__Blabla.cmi

  $ ls _build/default/mydummylib/*.cma
  _build/default/mydummylib/mydummylib.cma

  $ dir=_build/default/foo/.foo.objs/byte
  $ ls $dir/*.cmi
  _build/default/foo/.foo.objs/byte/foo__.cmi
  _build/default/foo/.foo.objs/byte/foo__Bar.cmi
  $ ls $dir/*.cmo
  _build/default/foo/.foo.objs/byte/foo__.cmo
  _build/default/foo/.foo.objs/byte/foo__Bar.cmo
