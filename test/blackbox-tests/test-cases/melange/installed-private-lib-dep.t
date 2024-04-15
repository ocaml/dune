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

Build the app that depends on `foo`, which in turn depends on a private lib

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build @melange --root app
  Entering directory 'app'
  Leaving directory 'app'

