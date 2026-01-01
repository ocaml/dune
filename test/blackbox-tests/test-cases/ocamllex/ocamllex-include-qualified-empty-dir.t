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
  File "bar-baz", line 1, characters 0-0:
  Error: "bar-baz" is an invalid module name.
  Module names must be non-empty, start with a letter, and composed only of the
  following characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: bar_baz would be a correct module name
  [1]

Adding an unrelated module in a different source tree should also ignore the
lexer with the invalid name.

  $ mkdir -p bar
  $ cat > bar/dune <<EOF
  > (ocamllex lexer)
  > EOF

  $ cat > bar/lexer.mll <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF

  $ cat > bar/lex-er.mll <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF

  $ dune build foo.cma
  File "bar-baz", line 1, characters 0-0:
  Error: "bar-baz" is an invalid module name.
  Module names must be non-empty, start with a letter, and composed only of the
  following characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: bar_baz would be a correct module name
  [1]

  $ mkdir -p bar
  $ cat > bar/dune <<EOF
  > (ocamllex lex-er)
  > EOF

  $ dune build foo.cma
  File "bar-baz", line 1, characters 0-0:
  Error: "bar-baz" is an invalid module name.
  Module names must be non-empty, start with a letter, and composed only of the
  following characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: bar_baz would be a correct module name
  [1]
