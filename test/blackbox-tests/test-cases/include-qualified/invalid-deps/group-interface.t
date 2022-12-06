We shouldn't allow foo/$x.ml to depend on foo/foo.ml

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > EOF

  $ cat > dune << EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo))
  > EOF

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
