The use of (include_subdirs qualified) should be compatible with the use of
Menhir in other places than the root of the file hierarchy.

  $ test() {
  > cat >dune <<EOF
  > (include_subdirs $1)
  > (executable (name foo))
  > EOF
  > dune build foo.exe
  > }

  $ test unqualified
  File "foo.ml", line 2, characters 2-14:
  2 |   Bar.Baz.unit (fun _ -> Bar.Baz.EOF) (Lexing.from_string "");
        ^^^^^^^^^^^^
  Error: Unbound module Bar
  Hint: Did you mean Baz?
  [1]
  $ test qualified
  File "bar/dune", line 1, characters 0-23:
  1 | (menhir
  2 |  (modules baz))
  Error: I can't determine what library/executable the files produced by this
  stanza are part of.
  [1]
