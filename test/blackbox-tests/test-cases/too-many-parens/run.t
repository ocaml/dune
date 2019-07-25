Check that the error messages produced when using too many parentheses
are readable.

  $ dune build --root a
  Entering directory 'a'
  File "dune", line 2, characters 2-14:
  2 |   (name hello)
        ^^^^^^^^^^^^
  Error: Atom expected
  [1]

  $ dune build --root b
  Entering directory 'b'
  File "dune", line 3, characters 13-16:
  3 |  (libraries (lib)))
                   ^^^
  Error: 'select' expected
  [1]

  $ dune build --root c
  Entering directory 'c'
  File "dune", line 3, characters 8-9:
  3 |  (deps (x y z)))
              ^
  Error: Unknown constructor x
  [1]

Checking that extra long stanzas (over 10 lines) are truncated in the middle, and the two blocks are aligned.
  $ dune build --root d
  Entering directory 'd'
  File "dune", line 3, characters 14-15:
  3 |   (libraries (a
                    ^
  Error: 'select' expected
  [1]

When the inner syntax is wrong, do not warn about the parens:

  $ dune build --root e
  Entering directory 'e'
  File "dune", line 3, characters 8-12:
  3 |  (deps (glob *)) ; this form doesn't exist
              ^^^^
  Error: Unknown constructor glob
  [1]
