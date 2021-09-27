Private libraries attached to packages shouldn't be displayed in the index

  $ cat <<EOF > dune-project
  > (lang dune 3.0)
  > (package (name foo))
  > EOF

  $ cat <<EOF > dune
  > (library
  >  (name foo)
  >  (package foo))
  > EOF
  > touch foo.ml bar.ml

  $ dune build @doc
  $ cat _build/default/_doc/_mlds/foo/index.mld
  {0 foo index}
