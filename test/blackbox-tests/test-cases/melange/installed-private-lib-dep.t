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
  $ dune install --prefix $PWD/prefix --root lib --display short
  Installing $TESTCASE_ROOT/prefix/lib/foo/META
  Installing $TESTCASE_ROOT/prefix/lib/foo/__private__/priv/melange/.public_cmi_melange/priv.cmi
  Installing $TESTCASE_ROOT/prefix/lib/foo/__private__/priv/melange/.public_cmi_melange/priv.cmt
  Installing $TESTCASE_ROOT/prefix/lib/foo/__private__/priv/melange/priv.cmj
  Installing $TESTCASE_ROOT/prefix/lib/foo/__private__/priv/melange/priv.ml
  Installing $TESTCASE_ROOT/prefix/lib/foo/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.ml

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

A public alias into the installed private library works in OCaml, but Melange
omits the private library's hidden CMI directory on supported compilers.
Direct references to the private library remain unavailable in both modes.

  $ if ocamlc -H . -version > /dev/null 2>&1; then
  >   sed -i.bak 's/(name foo)/(name consumer)/' app/dune-project &&
  >   sed -i.bak 's/(modes melange)/(modes byte melange)/' \
  >     lib/priv/dune lib/lib/dune &&
  >   printf '\nmodule P = Priv\n' >> lib/lib/foo.ml &&
  >   dune build @install --root lib &&
  >   dune install --prefix "$PWD/prefix" --root lib --display quiet &&
  >   printf '(library (name entry) (modes byte melange) (libraries foo))\n' \
  >     > app/dune &&
  >   printf 'let x = Foo.P.x\n' > app/entry.ml &&
  >   export OCAMLPATH="$PWD/prefix/lib/:$OCAMLPATH" &&
  >   dune build .entry.objs/byte/entry.cmo --root app &&
  >   ! dune build .entry.objs/melange/entry.cmj --root app > errors 2>&1 &&
  >   grep -q 'Foo.P is an alias for module Priv, which is missing$' errors &&
  >   printf 'let x = Priv.x\n' > app/entry.ml &&
  >   ! dune build .entry.objs/byte/entry.cmo --root app > errors 2>&1 &&
  >   grep -q '^Error: Unbound module Priv$' errors &&
  >   ! dune build .entry.objs/melange/entry.cmj --root app > errors 2>&1 &&
  >   grep -q '^Error: Unbound module Priv$' errors
  > fi
