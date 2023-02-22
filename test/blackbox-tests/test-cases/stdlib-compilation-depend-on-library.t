Test dependency on installed package

  $ mkdir a b prefix

  $ cat > a/dune-project <<EOF
  > (lang dune 3.7)
  > (package (name a))
  > (using experimental_building_ocaml_compiler_with_dune 0.1)
  > EOF
  $ cat > a/dune <<EOF
  > (library
  >  (name a)
  >  (public_name a)
  >  (stdlib
  >   (modules_before_stdlib CamlinternalFormatBasics)
  >   (internal_modules Camlinternal*)))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ cat > a/a.ml <<EOF
  > module Foo = A__Foo
  > EOF

  $ dune build --root a
  Entering directory 'a'
  Leaving directory 'a'

  $ dune install --root a --prefix $PWD/prefix
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/a.a
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cma
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cmx
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cmxa
  Installing $TESTCASE_ROOT/prefix/lib/a/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/a__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/a__Foo.cmx
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/a/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cmxs

  $ cat >b/dune-project <<EOF
  > (lang dune 3.7)
  > (package (name b))
  > EOF

  $ cat > b/dune <<EOF
  > (library
  >  (public_name b)
  >  (libraries a))
  > EOF

  $ cat > b/bar.ml <<EOF
  > let x = A.Foo.x
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root b @install --display=short
  Entering directory 'b'
        ocamlc .b.objs/byte/b.{cmi,cmo,cmt}
      ocamldep .b.objs/b__Bar.impl.d
      ocamlopt .b.objs/native/b.{cmx,o}
  File "$TESTCASE_ROOT/prefix/lib/a/dune-package", line 40, characters 14-38:
  40 |    (unwrapped CamlinternalFormatBasics))))
                     ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: List expected
  -> required by library "b" in _build/default
  -> required by _build/default/META.b
  -> required by _build/install/default/lib/b/META
  -> required by _build/default/b.install
  -> required by alias install
  Leaving directory 'b'
  [1]

