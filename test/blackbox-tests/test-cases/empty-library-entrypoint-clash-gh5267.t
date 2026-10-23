A generated alias for an empty library must not use the interface of an
executable module with the same name. Recompiling it after that interface is
copied currently fails, even though the same alias builds before the copy.

  $ make_dune_project 1.2
  $ cat >dune<<EOF
  > (library
  >  (name foo)
  >  (modules))
  > (executable
  >  (name bar)
  >  (libraries foo))
  > EOF
  $ cat >bar.ml <<EOF
  > module M = Foo
  > EOF
  $ touch foo.ml foo.mli

The generated alias builds while the unrelated interface is absent from the
build directory.

  $ dune build .foo.objs/byte/foo.cmi --sandbox=none
  $ test ! -e _build/default/foo.mli

Copy that interface, then change profiles to require recompilation rather than
reusing the successful cached action.

  $ dune build foo.mli
  $ dune build .foo.objs/byte/foo.cmi --profile release --sandbox=none
  File "foo.ml-gen", line 1:
  Error: Could not find the .cmi file for interface foo.mli.
  [1]
