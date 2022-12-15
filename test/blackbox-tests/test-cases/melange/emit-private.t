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
  >  (package a))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build

  $ dune install --prefix $PWD/prefix
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/.public_cmi/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/.public_cmi/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/.public_cmi/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/.public_cmi/a__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/a.a
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/a.cma
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/a.cmx
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/a.cmxa
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/a__Foo.cmx
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/a/__private__/a/a.cmxs

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
          melc b/.dist.mobjs/melange/melange__Bar.{cmi,cmj,cmt} (exit 2)
  File "b/bar.ml", line 1, characters 15-22:
  1 | let x = Js.log A.Foo.x
                     ^^^^^^^
  Error: Unbound module A
  File "a/dune", line 1, characters 0-32:
  1 | (library
  2 |  (name a)
  3 |  (package a))
  Error: No rule found for a/.a.objs/melange/a.cmj
  File "a/dune", line 1, characters 0-32:
  1 | (library
  2 |  (name a)
  3 |  (package a))
  Error: No rule found for a/.a.objs/melange/a__Foo.cmj
  [1]

$ node b/_build/default/dist/bar.js
foo
