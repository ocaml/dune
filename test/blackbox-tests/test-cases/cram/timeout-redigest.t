Testing how timeout affects the digest:

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat > mytest.t

This test counts the occurances of the cram script in the log.
  $ check() {
  >   cat _build/log | grep -c main.sh
  > }

We can observe the test is run the first time:

  $ dune test mytest.t
  $ check
  1

And is not run the second time:

  $ dune test mytest.t
  $ check
  0
  [1]

If we add a timeout, we would not expect for the digest of the cram test to
change.

  $ cat > dune <<EOF
  > (cram
  >  (timeout 1))
  > EOF

However this is currently not the case and we rerun the cram test:

  $ dune test mytest.t
  $ check
  1

  $ dune test mytest.t
  $ check
  0
  [1]

This is again the case on another time change:

  $ cat > dune <<EOF
  > (cram
  >  (timeout 2))
  > EOF
 
  $ dune test mytest.t
  $ check
  1

  $ dune test mytest.t
  $ check
  0
  [1]

