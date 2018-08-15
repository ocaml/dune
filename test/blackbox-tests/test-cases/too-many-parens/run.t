Check that the error messages produced when using too many parentheses
are readable.

  $ dune build --root a
  File "dune", line 1, characters 12-72:
  1: (executable (
  2:   (name hello)
  3:   (public_name hello)
  4:   (libraries (lib))
  5: ))
  These parentheses are no longer necessary with dune, please remove them.
  [1]

  $ dune build --root b
  File "dune", line 4, characters 12-17:
   (libraries (lib)))
              ^^^^^
  These parentheses are no longer necessary with dune, please remove them.
  [1]

  $ dune build --root c
  File "dune", line 3, characters 7-14:
   (deps (x y z)))
         ^^^^^^^
  These parentheses are no longer necessary with dune, please remove them.
  [1]

Checking that extra long stanzas (over 10 lines) are not printed
  $ dune build --root d
  File "dune", line 3, characters 13-192:
  These parentheses are no longer necessary with dune, please remove them.
  [1]
