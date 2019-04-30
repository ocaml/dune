`dune install` should handle destination directories that don't exist

  $ cat > dune <<EOF
  > (install
  >  (section man)
  >  (files
  >     (man-page-a.1 as man-page-a.%{context_name}.1) ; incorrect usage!
  >     (man-page-b.1 as man1/man-page-b.%{context_name}.1)
  >     another-man-page.3)
  > )
  > EOF

  $ dune build @install
  $ dune install --prefix install --libdir lib
  Installing install/lib/foo/META
  Installing install/lib/foo/dune-package
  Installing install/lib/foo/opam
  Installing install/man/man-page-a.default.1
  Installing install/man/man1/man-page-b.default.1
  Installing install/man/man3/another-man-page.3

  $ cat foo.install | grep man
  man: [
    "_build/install/default/man/man-page-a.default.1" {"man-page-a.default.1"}
    "_build/install/default/man/man1/man-page-b.default.1" {"man1/man-page-b.default.1"}
    "_build/install/default/man/man3/another-man-page.3"
