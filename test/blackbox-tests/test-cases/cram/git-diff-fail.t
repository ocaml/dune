We demonstrate the leaking of the display of internal processes when running
cram tests.

First we make a cram test:

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > EOF

  $ cat > mytest.t << EOF
  >   $ echo A
  >   B
  > EOF

We need to avoid the special treatment of the test when INSIDE_DUNE is set:
  $ unset INSIDE_DUNE

We get nonsense internal output in the display:

  $ dune build --root=. --diff-command="exit 1" --display=short @runtest 2>&1 >/dev/null | head -n2
  File "mytest.t", line 1, characters 0-0:
            sh (internal) (exit 1)
