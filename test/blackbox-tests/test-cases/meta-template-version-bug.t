This test demonstrates a bug when there's a package with a meta template and a
custom version:

  $ git init -q
  $ touch foo
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
  > # DUNE_GEN
  > EOF

  $ dune build @install

  $ dune install --prefix ./_install 2>&1 | grep -v Installing
  [1]

  $ cat ./_install/lib/foobarlib/dune-package | \
  >   sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (use_meta)

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
  Leaving directory 'external'
  foobarlib
