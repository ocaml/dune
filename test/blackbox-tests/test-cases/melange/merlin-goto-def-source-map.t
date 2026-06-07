Show Melange-specific Merlin configuration for source lookup

  $ cat > dune-project <<EOF
  > (lang dune 3.24)
  > (using melange 1.0)
  > EOF
  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (libraries lib)
  >  (modules main))
  > EOF
  $ cat > main.ml <<EOF
  > let _ = Foo.foo
  > EOF
  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name lib)
  >  (wrapped false)
  >  (modes melange)
  >  (preprocess (pps melange.ppx)))
  > EOF
  $ cat > lib/foo.ml <<EOF
  > let foo = "ocaml"
  > EOF
  $ cat > lib/foo.melange.ml <<EOF
  > let foo = "melange"
  > EOF

  $ dune build @all @check
  $ dune ocaml dump-dot-merlin "$PWD" > merlin.conf
  $ cat merlin.conf | grep '\.melange_src'
  S $TESTCASE_ROOT/lib/.melange_src
  $ cat merlin.conf | grep '^SUFFIX '
  [1]
