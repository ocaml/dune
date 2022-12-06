We shouldn't allow foo/y/$x.ml to depend on foo/foo.ml

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > EOF

  $ cat > dune << EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo))
  > EOF

  $ mkdir -p x/y
  $ touch x/x.ml
  $ cat >x/y/z.ml <<EOF
  > let () = X.f
  > EOF

  $ dune build
  Error: Module Z in directory _build/default depends on X.
  This doesn't make sense to me.
  
  X is the main module of the library and is the only module exposed outside of
  the library. Consequently, it should be the one depending on all the other
  modules in the library.
  -> required by _build/default/.foo.objs/foo__X__Y__Z.impl.all-deps
  -> required by _build/default/.foo.objs/byte/foo__X__Y__Z.cmo
  -> required by _build/default/foo.cma
  -> required by alias all
  -> required by alias default
  [1]
