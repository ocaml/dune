There is ambiguity between the two different foo modules.
The behaviour is correct in the bootstrap process, but not in dune.
It picks up the wrong one.

  $ echo "(lang dune 3.21)" > dune-project
  $ cat > dune << EOF
  > (include_subdirs qualified)
  > (executable
  >  (name main))
  > EOF

  $ cat > main.ml << EOF
  > let ()  = print_endline Bar.Baz.exported
  > EOF

  $ cat > foo.ml << EOF
  > let msg = "No"
  > EOF

  $ mkdir bar
  $ cat > bar/foo.ml << EOF
  > let msg = "Success"
  > EOF

  $ cat > bar/baz.ml << EOF
  > let exported = Foo.msg ^ "!"
  > EOF

This is wrong, it should pick the closest module named foo.
  $ dune exec ./main.exe
  File "bar/baz.ml", line 1, characters 15-22:
  1 | let exported = Foo.msg ^ "!"
                     ^^^^^^^
  Error: The module Foo is an alias for module Dune__exe__Foo, which is missing
  [1]
