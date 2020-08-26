This test demonstrates a bug when there's a package with a meta template and a
custom version:

  $ git init -q
  $ git add .
  $ git commit -qm _
  $ git tag -a 1.0 -m 1.0

  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > (package (name foobarlib))
  > EOF

  $ cat >foobarlib.ml <<EOF
  > let foo () =
  >   print_endline "foobarlib"
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (public_name foobarlib))
  > EOF

  $ cat >META.foobarlib.template <<EOF
  > EOF

  $ dune build @install

  $ cat _build/install/default/lib/foobarlib/dune-package
  (lang dune 2.7)
  (use_meta)

  $ dune install --prefix ./_install 2>&1 | grep -v Installing
  [1]

  $ mkdir external
  $ echo "(lang dune 2.7)" > external/dune-project
  $ cat >external/main.ml <<EOF
  > let () = Foobarlib.foo ()
  > EOF
  $ cat > external/dune <<EOF
  > (executable (name main) (libraries foobarlib))
  > EOF

  $ OCAMLPATH=$PWD/_install/lib dune exec --root external ./main.exe
  Entering directory 'external'
  Entering directory 'external'
  File "$TESTCASE_ROOT/_install/lib/foobarlib/dune-package", line 3, characters 1-8:
  3 | (version 1.0)
       ^^^^^^^
  Error: Unknown field version
  [1]
