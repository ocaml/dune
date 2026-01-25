We test the behavior of watch mode when we have multiple errors

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
  --- x
  +++ y
  @@ -1 +1 @@
  -not so
  +different
  \ No newline at end of file
  File "x", line 1, characters 0-0:
  --- x
  +++ z
  @@ -1 +1 @@
  -not so
  +different
  \ No newline at end of file
  Had 2 errors, waiting for filesystem changes...
