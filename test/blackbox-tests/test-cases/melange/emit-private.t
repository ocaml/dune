Test dependency on a private library in the same package as melange.emit

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > (package  (name a))
  > (using melange 0.1)
  > EOF

  $ mkdir a b prefix

  $ cat > a/dune <<EOF
  > (library
  >  (name a)
  >  (modes melange)
  >  (package a))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build

  $ dune install --prefix $PWD/prefix
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/.public_cmi_melange/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/.public_cmi_melange/a.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/.public_cmi_melange/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/.public_cmi_melange/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/.public_cmi_melange/a__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/.public_cmi_melange/a__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package

  $ cat > b/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias dist)
  >  (libraries a)
  >  (package a)
  >  (module_system commonjs))
  > EOF

  $ cat > b/bar.ml <<EOF
  > let x = Js.log A.Foo.x
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build @dist --display=short
          melc b/dist/a/a.js
          melc b/.dist.mobjs/melange/melange__Bar.{cmi,cmj,cmt}
          melc b/dist/a/foo.js
          melc b/dist/b/bar.js

  $ node _build/default/b/dist/b/bar.js
  foo
