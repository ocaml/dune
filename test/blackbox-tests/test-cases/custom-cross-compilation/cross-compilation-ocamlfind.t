In a cross-compilation scenario, Dune should be able to find libraries defined
in the default findlib.conf

  $ mkdir -p etc/findlib.conf.d
  $ export OCAMLFIND_CONF=$PWD/etc/findlib.conf
  $ cat >etc/findlib.conf <<EOF
  > path="$PWD/prefix/lib"
  > ocamldep="$PWD/notocamldep"
  > EOF
  $ cat >etc/findlib.conf.d/foo.conf <<EOF
  > path(foo)=""
  > ocamldep(foo)="$PWD/notocamldep-foo"
  > EOF

  $ cat >notocamldep <<EOF
  > #!/usr/bin/env sh
  > ocamldep "\$@"
  > EOF
  $ cat >notocamldep-foo <<EOF
  > #!/usr/bin/env sh
  > ocamldep "\$@"
  > EOF
  $ chmod +x notocamldep notocamldep-foo

  $ mkdir lib
  $ cat > lib/dune-project <<EOF
  > (lang dune 3.7)
  > (package (name libdep))
  > EOF
  $ cat > lib/dune <<EOF
  > (library (public_name libdep))
  > EOF
  $ cat > lib/libdep.ml <<EOF
  > let x = 1
  > EOF

  $ dune build --root lib @install
  Entering directory 'lib'
  Leaving directory 'lib'

  $ dune install --root lib --prefix $PWD/prefix

  $ mkdir app
  $ cat > app/dune-project <<EOF
  > (lang dune 3.7)
  > (package (name repro))
  > EOF
  $ cat > app/dune <<EOF
  > (executable
  >  (name gen)
  >  (modules gen)
  >  (enabled_if
  >   (= %{context_name} "default"))
  >  (libraries libdep))
  > (rule
  >  (with-stdout-to
  >   gen.ml
  >   (echo "let () = Format.printf \"let x = 1\"")))
  > (library
  >  (name repro)
  >  (public_name repro)
  >  (modules foo))
  > (rule
  >  (with-stdout-to
  >   foo.ml
  >   (run ./gen.exe)))
  > EOF

ocamlfind can find it

  $ ocamlfind list | grep libdep
  libdep              (version: n/a)

Dune should be able to find it too

  $ dune build --root=app @install -x foo --verbose 2>&1 | grep notocamldep-foo
  Running[8]: (cd _build/default.foo && $TESTCASE_ROOT/notocamldep-foo -modules -impl foo.ml) > _build/default.foo/.repro.objs/repro__Foo.impl.d

Library is built in the target context

  $ ls app/_build/default.foo
  META.repro
  foo.ml
  repro-foo.install
  repro.a
  repro.cma
  repro.cmxa
  repro.cmxs
  repro.dune-package
  repro.ml-gen

Executable was built in the host context

  $ ls app/_build/default
  gen.exe
  gen.ml
  gen.mli
