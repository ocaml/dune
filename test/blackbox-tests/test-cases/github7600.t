Reproducing bug in https://github.com/ocaml/dune/issues/7600

When using (include_subdirs qualified), valid module names should be checked for
directories too.


  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > EOF

  $ mkdir src

  $ cat > src/dune << EOF
  > (include_subdirs qualified)
  > (executable (name foo))
  > EOF

  $ cat > src/foo.ml

  $ mkdir src/baz-bar

  $ dune build @install
  File "src/baz-bar", line 1, characters 0-0:
  Error: "baz-bar" is an invalid module name.
  Module names must be non-empty, start with a letter, and composed only of the
  following characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: baz_bar would be a correct module name
  [1]
