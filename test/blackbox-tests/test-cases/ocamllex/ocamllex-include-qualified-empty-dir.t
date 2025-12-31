Test `(include_subdirs qualified)` in the presence of invalid module name
directories that don't contain source files

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat > dune <<EOF
  > (include_subdirs qualified)
  > (library (name foo))
  > EOF
  $ mkdir -p bar-baz
  $ echo hello > bar-baz/some-data.txt

There's no corresponding `(ocamllex ..)` stanza for `lexer.mll`, so it should
not be considered as a module source.

  $ cat > bar-baz/lexer.mll <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF

The directory `bar-baz`, even though not a valid module name, doesn't have any
source files. The library should still compile.

  $ dune build foo.cma
