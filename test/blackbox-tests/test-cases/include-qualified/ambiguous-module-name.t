There is ambiguity between the two different foo modules.
The behaviour is now correct both in the bootstrap process, and in dune.
It picks up the closest one.

  $ make_dune_project 3.21
  $ cat > dune << EOF
  > (include_subdirs qualified)
  > (executable
  >  (name main))
  > EOF

  $ cat > main.ml << EOF
  > let ()  = print_endline Bar.Baz.exported
  > EOF

  $ cat > foo.ml << EOF
  > let msg = "Failure"
  > EOF

  $ mkdir bar
  $ cat > bar/foo.ml << EOF
  > let msg = "Success"
  > EOF

  $ cat > bar/baz.ml << EOF
  > let exported = Foo.msg ^ "!"
  > EOF

This is now fixed.
  $ dune exec ./main.exe
  Success!
