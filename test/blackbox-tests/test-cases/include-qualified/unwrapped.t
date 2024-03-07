Unwrapped libraries and (include_subdirs unqualified) 

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF

  $ mkdir lib/ && cd lib

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (wrapped false)
  >  (name foo))
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

  $ cd ..

  $ cat >dune <<EOF
  > (executable
  >  (libraries foo)
  >  (name main))
  > EOF

  $ cat >main.ml <<EOF
  > let () = X.run (); Y.Z.run ()
  > EOF

  $ dune exec ./main.exe
  Foo
  Z
