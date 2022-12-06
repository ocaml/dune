We should forbid lib interfaces modules from depending on themselves:

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > EOF

  $ cat > dune << EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo))
  > EOF

  $ cat > foo.ml <<EOF
  > let () = Foo.f ()
  > EOF

  $ touch bar.ml

  $ dune build @check
  File "foo.ml", line 1, characters 9-14:
  1 | let () = Foo.f ()
               ^^^^^
  Error: Unbound module Foo
  [1]

We also forbid submodules from depending on their interface modules:

  $ rm foo.ml bar.ml
  $ mkdir baz
  $ touch baz/baz.ml
  $ cat >baz/bar.ml <<EOF
  > let () = Baz.f
  > EOF

  $ dune build
  Error: Module Bar in directory _build/default depends on Baz.
  This doesn't make sense to me.
  
  Baz is the main module of the library and is the only module exposed outside
  of the library. Consequently, it should be the one depending on all the other
  modules in the library.
  -> required by _build/default/.foo.objs/foo__Baz__Bar.impl.all-deps
  -> required by _build/default/.foo.objs/byte/foo__Baz__Bar.cmo
  -> required by _build/default/foo.cma
  -> required by alias all
  -> required by alias default
  [1]

Or their parent interface modules:

  $ rm -rf baz
  $ touch baz.ml
  $ mkdir -p baz/foo/
  $ cat >baz/foo/z.ml <<EOF
  > let () = Baz.f
  > EOF
  $ dune build
  Error: Module Z in directory _build/default depends on Baz.
  This doesn't make sense to me.
  
  Baz is the main module of the library and is the only module exposed outside
  of the library. Consequently, it should be the one depending on all the other
  modules in the library.
  -> required by _build/default/.foo.objs/foo__Baz__Foo__Z.impl.all-deps
  -> required by _build/default/.foo.objs/byte/foo__Baz__Foo__Z.cmo
  -> required by _build/default/foo.cma
  -> required by alias all
  -> required by alias default
  [1]
