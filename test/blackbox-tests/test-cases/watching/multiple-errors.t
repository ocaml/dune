We test the behavior of watch mode when we have multiple errors

  $ . ./helpers.sh

  $ echo "(lang dune 3.11)" > dune-project

  $ start_dune

  $ cat > x <<EOF
  > not so
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (action
  >   (progn
  >    (write-file y "different")
  >    (diff x y))))
  > (rule
  >  (action
  >   (progn
  >    (write-file z "different")
  >    (diff x z))))
  > (rule
  >  (deps y z)
  >  (action
  >   (write-file w "")))
  > EOF

  $ build w
  Failure

  $ stop_dune
  File "x", line 1, characters 0-0:
  Error: Files _build/default/x and _build/default/y differ.
  File "x", line 1, characters 0-0:
  Error: Files _build/default/x and _build/default/z differ.
  Had 2 errors, waiting for filesystem changes...
