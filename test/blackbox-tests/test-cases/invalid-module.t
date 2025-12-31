Test `(include_subdirs qualified)` in the presence of invalid module names in
the source tree

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF
  $ cat > dune <<EOF
  > (library (name foo) (modules foo))
  > EOF
  $ touch foo.ml

Add `invalid-module.ml`, present in the source tree but not part of any
artifacts

  $ touch invalid-module.ml

  $ dune build
