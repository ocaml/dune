Test dependency on a private library in the same package as melange.emit

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package  (name pkg))
  > (using melange 0.1)
  > EOF

  $ mkdir a b prefix

  $ cat > a/dune <<EOF
  > (library
  >  (name a)
  >  (modes melange)
  >  (package pkg))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build

  $ dune install --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/pkg/META
  Installing $TESTCASE_ROOT/prefix/lib/pkg/__private__/a/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/pkg/__private__/a/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/pkg/__private__/a/melange/.public_cmi_melange/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/pkg/__private__/a/melange/.public_cmi_melange/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/pkg/__private__/a/melange/.public_cmi_melange/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/pkg/__private__/a/melange/.public_cmi_melange/a__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/pkg/__private__/a/melange/a.cmj
  Installing $TESTCASE_ROOT/prefix/lib/pkg/__private__/a/melange/a__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/pkg/dune-package

  $ cat > b/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias dist)
  >  (libraries a)
  >  (emit_stdlib false)
  >  (package pkg))
  > EOF

  $ cat > b/bar.ml <<EOF
  > let x = Js.log A.Foo.x
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build @dist --display=short 2>&1 | grep -v melange
          melc b/dist/node_modules/pkg.__private__.a/a.js
          melc b/dist/node_modules/pkg.__private__.a/foo.js
          melc b/dist/b/bar.js

  $ node _build/default/b/dist/b/bar.js
  foo
