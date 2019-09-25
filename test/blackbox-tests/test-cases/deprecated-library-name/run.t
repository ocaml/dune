Test the `deprecated_library_name` stanza
=========================================

Test that deprecated library names are supported in a given workspace
---------------------------------------------------------------------

  $ for i in a b c; do
  >   mkdir $i
  >   cat >$i/dune-project <<EOF
  > (lang dune 2.0)
  > (package (name $i))
  > EOF
  > done

  $ cat >a/dune <<EOF
  > (deprecated_library_name (old_public_name a) (new_public_name b))
  > EOF

  $ cat >b/dune <<EOF
  > (library (public_name b))
  > EOF

  $ cat >b/b.ml <<EOF
  > let x = "Hello, world!"
  > EOF

  $ cat >c/dune <<EOF
  > (executable (name prog) (libraries a))
  > EOF

  $ cat >c/prog.ml <<EOF
  > print_endline B.x
  > EOF

  $ dune exec c/prog.exe
  Hello, world!

Test that deprecated library names can be installed
---------------------------------------------------

  $ dune clean

Note that at this point the library "b" does not exist, so this also
tests that the "old_public_name" field is evaluated lazily

  $ cd a
  > dune build @install --root .
  > dune install --prefix $PWD/../_install
  Installing $TESTCASE_ROOT/a/../_install/lib/a/META
  Installing $TESTCASE_ROOT/a/../_install/lib/a/dune-package

  $ rm -rf a

  $ cat $PWD/_install/lib/a/META
  requires = "b"

  $ cat $PWD/_install/lib/a/dune-package
  (lang dune 2.0)
  (name a)
  (deprecated_library_name (old_public_name a) (new_public_name b))

Now we install "b". We do need to install it as an installed
deprecated library will be resolved in the installed world only.

  $ cd b
  > dune build @install --root .
  > dune install --prefix $PWD/../_install
  Installing $TESTCASE_ROOT/b/../_install/lib/b/META
  Installing $TESTCASE_ROOT/b/../_install/lib/b/b$ext_lib
  Installing $TESTCASE_ROOT/b/../_install/lib/b/b.cma
  Installing $TESTCASE_ROOT/b/../_install/lib/b/b.cmi
  Installing $TESTCASE_ROOT/b/../_install/lib/b/b.cmt
  Installing $TESTCASE_ROOT/b/../_install/lib/b/b.cmx
  Installing $TESTCASE_ROOT/b/../_install/lib/b/b.cmxa
  Installing $TESTCASE_ROOT/b/../_install/lib/b/b.cmxs
  Installing $TESTCASE_ROOT/b/../_install/lib/b/b.ml
  Installing $TESTCASE_ROOT/b/../_install/lib/b/dune-package

  $ rm -rf b

  $ env OCAMLPATH=$PWD/_install/lib dune exec c/prog.exe
  Hello, world!

This error message could be improved to mention that it is in fact "b"
that wasn't found:

  $ rm -rf _install/lib/b

  $ env OCAMLPATH=$PWD/_install/lib dune exec c/prog.exe
  File "c/dune", line 1, characters 35-36:
  1 | (executable (name prog) (libraries a))
                                         ^
  Error: Library "a" not found.
  Hint: try: dune external-lib-deps --missing c/prog.exe
  [1]
