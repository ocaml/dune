Test what happens in watch mode when we depend on dot files

  $ . ./helpers.sh

  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (targets y)
  >  (deps .x)
  >  (action (system "cat .x > y")))
  > EOF

  $ start_dune

  $ echo 1 > .x
  $ build y
  Success
  $ cat _build/default/y
  1

  $ echo 2 > .x
  $ build y
  Success
  $ cat _build/default/y
  2

  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...

Same but in a sub-directory (the exclude regexp used to be sensitive to that):

  $ mkdir test
  $ mv dune test/dune

  $ start_dune

  $ echo 1 > test/.x
  $ build test/y
  Success
  $ cat _build/default/test/y
  1

  $ echo 2 > test/.x
  $ build test/y
  Success
  $ cat _build/default/test/y
  2

  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...

