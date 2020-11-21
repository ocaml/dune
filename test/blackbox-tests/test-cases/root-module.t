A library can be shadowed by an internal module name:

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > EOF

  $ mkdir lib0 lib1 lib2

  $ cat >lib0/dune <<EOF
  > (library
  >  (name lib0))
  > EOF
  $ cat >lib0/lib0.ml <<EOF
  > let greeting_from_lib0 = "Hello World"
  > EOF

  $ cat >lib1/dune <<EOF
  > (library
  >  (name lib1))
  > EOF
  $ cat >lib1/lib1.ml <<EOF
  > let greeting = "Hello World"
  > EOF

  $ cat >lib2/dune <<EOF
  > (library
  >  (libraries lib1)
  >  (name lib2))
  > EOF

Now we shadow lib1:
  $ cat >lib2/lib1.ml <<EOF
  > let greeting = ()
  > EOF
  $ cat >lib2/lib2.ml <<EOF
  > print_endline Lib1.greeting
  > EOF

  $ dune build @all
  File "lib2/lib2.ml", line 1, characters 14-27:
  1 | print_endline Lib1.greeting
                    ^^^^^^^^^^^^^
  Error: This expression has type unit but an expression was expected of type
           string
  [1]

We can use the rename dependency type to use lib1 with a different name:

  $ cat >lib2/dune <<EOF
  > (library
  >  (libraries lib1 unix)
  >  (root_module root)
  >  (name lib2))
  > EOF
  $ cat >lib2/lib2.ml <<EOF
  > module U = UnixLabels
  > let () = print_endline Root.Lib1.greeting
  > EOF
  $ dune build @all

The same for executables:

  $ mkdir exe
  $ cat >exe/dune <<EOF
  > (executable
  >  (name foo)
  >  (libraries lib1)
  >  (root_module root))
  > EOF
  $ echo >exe/lib1.ml <<EOF
  > let greeting = ()
  > EOF
  $ cat >exe/foo.ml <<EOF
  > print_endline Root.Lib1.greeting
  > EOF
  $ dune exec ./exe/foo.exe
  Hello World
