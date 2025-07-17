Here we check the validation of the timeout field of the cram stanza.

First we check the version guard.

  $ cat > dune-project <<EOF
  > (lang dune 3.19)
  > EOF

  $ cat > dune <<EOF
  > (cram
  >  (timeout 1))
  > EOF

  $ dune build
  File "dune", line 2, characters 1-12:
  2 |  (timeout 1))
       ^^^^^^^^^^^
  Error: 'timeout' is only available since version 3.20 of the dune language.
  Please update your dune-project file to have (lang dune 3.20).
  [1]

Next we check some invalid values:

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat > test.t <<EOF
  >   $ echo hi
  > EOF

Negative values fail immediately.

  $ cat > dune <<EOF
  > (cram
  >  (timeout -1.0))
  > EOF

  $ dune test test.t
  File "dune", line 2, characters 10-14:
  2 |  (timeout -1.0))
                ^^^^
  Error: Timeout value must be a non-negative float.
  [1]

Checking some currently accepted float values:

  $ test() {
  >   echo "(cram (timeout $1))" > dune
  >   dune build 
  > }

  $ test -1
  File "dune", line 1, characters 15-17:
  1 | (cram (timeout -1))
                     ^^
  Error: Timeout value must be a non-negative float.
  [1]
  $ test Inf
  $ test +Inf
  $ test -Inf
  File "dune", line 1, characters 15-19:
  1 | (cram (timeout -Inf))
                     ^^^^
  Error: Timeout value must be a non-negative float.
  [1]
  $ test nan
  File "dune", line 1, characters 15-18:
  1 | (cram (timeout nan))
                     ^^^
  Error: Timeout value must be a non-negative float.
  [1]
  $ test .5
  $ test 0.
  $ test 1.
  $ test 1.0e1
  $ test 1e1
  $ test 1e-1
  $ test 1e+1
  $ test 1e308
  $ test 1e-324

Invalid values should be vetted correctly:

  $ test
  File "dune", line 1, characters 6-16:
  1 | (cram (timeout ))
            ^^^^^^^^^^
  Error: Not enough arguments for "timeout"
  [1]

  $ test foo
  File "dune", line 1, characters 15-18:
  1 | (cram (timeout foo))
                     ^^^
  Error: Float expected
  [1]

  $ test --1
  File "dune", line 1, characters 15-18:
  1 | (cram (timeout --1))
                     ^^^
  Error: Float expected
  [1]

  $ test 1..0
  File "dune", line 1, characters 15-19:
  1 | (cram (timeout 1..0))
                     ^^^^
  Error: Float expected
  [1]

  $ test 1e
  File "dune", line 1, characters 15-17:
  1 | (cram (timeout 1e))
                     ^^
  Error: Float expected
  [1]

  $ test 1.0.0
  File "dune", line 1, characters 15-20:
  1 | (cram (timeout 1.0.0))
                     ^^^^^
  Error: Float expected
  [1]
