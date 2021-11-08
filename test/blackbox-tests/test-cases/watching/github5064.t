A test case for #5064: errors when changing module dependencies in the
file-watching mode.

  $ . ./helpers.sh

  $ echo '(lang dune 3.0)' > dune-project
  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library (name lib))
  > EOF
  $ cat > dune <<EOF
  > (executable (name x) (libraries lib))
  > EOF
  $ echo 'let hello = "Hello"' > lib/a.ml
  $ echo 'let world = "World"' > lib/b.ml
  $ echo 'let message = A.hello ^ ", " ^ B.world' > lib/lib.ml

  $ echo 'print_endline Lib.message' > x.ml

  $ start_dune x.exe

  $ dune_wait
  Success
  $ _build/default/x.exe
  Hello, World

Now let's make [lib/a.ml] depend on [lib/b.ml]. It doesn't work!

  $ cat > lib/a.ml <<EOF
  > let _ = B.world
  > let hello = "Hello"
  > EOF

  $ dune_wait
  Failure

Let's try a manual restart.

  $ stop_dune
  Success, waiting for filesystem changes...
  File "_none_", line 1:
  Error: No implementations provided for the following modules:
           Lib__B referenced from lib/lib.cmxa(Lib__A)
  Had errors, waiting for filesystem changes...

  $ start_dune x.exe

It works now!

  $ dune_wait
  Success

We're done.

  $ stop_dune
  Success, waiting for filesystem changes...
