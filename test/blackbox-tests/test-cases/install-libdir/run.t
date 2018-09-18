`dune install` should handle destination directories that don't exist

  $ dune build @install
  $ dune install --prefix install --libdir lib
  Installing install/lib/foo/META
  Installing install/lib/foo/foo$ext_lib
  Installing install/lib/foo/foo.cma
  Installing install/lib/foo/foo.cmi
  Installing install/lib/foo/foo.cmt
  Installing install/lib/foo/foo.cmx
  Installing install/lib/foo/foo.cmxa
  Installing install/lib/foo/foo.cmxs
  Installing install/lib/foo/foo.dune
  Installing install/lib/foo/foo.ml
  Installing install/lib/foo/opam
