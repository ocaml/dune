Unwrapped executable and `(include_subdirs qualified)`

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (wrapped_executables false)
  > EOF

  $ mkdir x
  $ cat >x/x.ml <<EOF
  > let run () = Foo.run ()
  > EOF
  $ cat >x/foo.ml <<EOF
  > let run () = print_endline "Foo"
  > EOF

  $ mkdir y
  $ cat >y/z.ml <<EOF
  > let run () = print_endline "Z"
  > EOF

  $ cat >main.ml <<EOF
  > let () = X.run (); Y.Z.run ()
  > EOF
  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (executable (name main))
  > EOF

  $ dune exec ./main.exe
  Foo
  Z
