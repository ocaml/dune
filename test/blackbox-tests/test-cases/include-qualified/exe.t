
  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (executable
  >  (name foo))
  > EOF

  $ mkdir -p bar/baz/ baz/
  $ touch bar/baz/baz.ml baz/bar.ml
  $ cat >foo.ml <<EOF
  > module X = Baz.Bar
  > module Y = Bar.Baz
  > EOF

  $ dune exec ./foo.exe
