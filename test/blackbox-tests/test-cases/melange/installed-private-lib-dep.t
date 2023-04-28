Melange (installed) library depends on private library

  $ mkdir -p lib/lib lib/priv
  $ cat > lib/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name foo))
  > (using melange 0.1)
  > EOF

  $ cat > lib/priv/dune <<EOF
  > (library
  >  (name priv)
  >  (package foo)
  >  (modes melange))
  > EOF
  $ cat > lib/priv/priv.ml <<EOF
  > let x = "private"
  > EOF

  $ cat > lib/lib/dune <<EOF
  > (library
  >  (public_name foo)
  >  (modes melange)
  >  (libraries priv))
  > EOF
  $ cat > lib/lib/foo.ml <<EOF
  > let x = "public lib uses " ^ Priv.x
  > EOF

  $ dune build @install --root lib
  Entering directory 'lib'
  Leaving directory 'lib'
  $ dune install --prefix $PWD/prefix --root lib --display short
  Installing $TESTCASE_ROOT/prefix/lib/foo/META
  Installing $TESTCASE_ROOT/prefix/lib/foo/__private__/priv/melange/.public_cmi_melange/priv.cmi
  Installing $TESTCASE_ROOT/prefix/lib/foo/__private__/priv/melange/.public_cmi_melange/priv.cmt
  Installing $TESTCASE_ROOT/prefix/lib/foo/__private__/priv/melange/priv.cmj
  Installing $TESTCASE_ROOT/prefix/lib/foo/__private__/priv/priv.ml
  Installing $TESTCASE_ROOT/prefix/lib/foo/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/foo/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmt

  $ mkdir app
  $ cat > app/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name foo))
  > (using melange 0.1)
  > EOF

  $ cat > app/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (libraries foo)
  >  (emit_stdlib false))
  > EOF

  $ cat > app/entry.ml <<EOF
  > let () = Js.log Foo.x
  > EOF

An issue similar to #7104 still present because the `.cmj` is not visible.

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build @melange --root app --display=short
  Entering directory 'app'
          melc output/node_modules/foo.__private__.priv/priv.js
          melc .output.mobjs/melange/melange__Entry.{cmi,cmj,cmt}
          melc output/entry.js
          melc output/node_modules/foo/foo.js (exit 2)
  File "_none_", line 1:
  Error: Priv not found, it means either the module does not exist or it is a namespace
  Leaving directory 'app'
  [1]

