We try to load a module defined in an executable

  $ cat >dune-project <<EOF
  > (lang dune 3.3)
  > EOF

  $ cat >bar.ml <<EOF
  > let v = 42
  > EOF

  $ cat >foo.ml <<EOF
  > let foo = Bar.v + 42
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name foo))
  > EOF

  $ dune ocaml top-module foo.ml
  #directory "$TESTCASE_ROOT/_build/.top/Foo.a6bb488f1a99";;
  #load "$TESTCASE_ROOT/_build/default/.foo.eobjs/byte/dune__exe__Bar.cmo";;
  #load "$TESTCASE_ROOT/_build/default/.foo.eobjs/byte/dune__exe.cmo";;
  #use "$TESTCASE_ROOT/_build/default/foo.ml";;

  $ dir=_build/default/.foo.eobjs/byte/
  $ ls $dir/*.cmi
  _build/default/.foo.eobjs/byte//dune__exe.cmi
  _build/default/.foo.eobjs/byte//dune__exe__Bar.cmi
  $ ls $dir/*.cmo
  _build/default/.foo.eobjs/byte//dune__exe.cmo
  _build/default/.foo.eobjs/byte//dune__exe__Bar.cmo
