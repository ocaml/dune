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
  2 |   Bar.Baz.unit (fun _ -> Bar.Baz.EOF) (Lexing.from_string "")
        ^^^^^^^^^^^^
  Error: Unbound module Bar
  Hint: Did you mean Baz?
  [1]
  $ test qualified
