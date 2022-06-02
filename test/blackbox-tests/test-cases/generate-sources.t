Generate the source of an executable in a subdir:

  $ cat >dune-project <<EOF
  > (lang dune 3.2)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (with-stdout-to foo/bar.ml (echo "let foo = 42;;")))
  > (include_subdirs unqualified)
  > (executable (name bar))
  > EOF

  $ dune exec ./bar.exe
  File ".bar.eobjs/native/_unknown_", line 1, characters 0-0:
  Error: No rule found for bar.ml
  [1]
