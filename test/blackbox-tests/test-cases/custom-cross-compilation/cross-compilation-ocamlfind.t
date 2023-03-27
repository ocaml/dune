In a cross-compilation scenario, Dune should be able to find libraries defined
in the default findlib.conf

  $ mkdir -p etc/findlib.conf.d
  $ export OCAMLFIND_CONF=$PWD/etc/findlib.conf
  $ cat >etc/findlib.conf <<EOF
  > path="$PWD/prefix/lib"
  > EOF
  $ cat >etc/findlib.conf.d/foo.conf <<EOF
  > path(foo)=""
  > EOF

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

  $ dune build --root=app @install -x foo
  Entering directory 'app'
  File "dune", line 6, characters 12-18:
  6 |  (libraries libdep))
                  ^^^^^^
  Error: Library "libdep" not found.
  -> required by _build/default/.gen.eobjs/byte/dune__exe__Gen.cmi
  -> required by _build/default/.gen.eobjs/native/dune__exe__Gen.cmx
  -> required by _build/default/gen.exe
  -> required by _build/default.foo/foo.ml
  -> required by _build/install/default.foo/lib/repro/foo.ml
  -> required by _build/default.foo/repro-foo.install
  -> required by alias install (context default.foo)
  Leaving directory 'app'
  [1]

