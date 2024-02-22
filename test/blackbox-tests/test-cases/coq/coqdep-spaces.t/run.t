Testing a simple composition of theories. We have two theories A and B and B
depends on A.

  $ dune build
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  File "lib B/dune", line 1, characters 0-54:
  1 | (coq.theory
  2 |  (name B)
  3 |  (package Simple)
  4 |  (theories A))
  Error: No rule found for "lib B/A/a1.vo"
  File "lib B/dune", line 1, characters 0-54:
  1 | (coq.theory
  2 |  (name B)
  3 |  (package Simple)
  4 |  (theories A))
  Error: No rule found for "lib B/A/a2.vo"
  File "lib B/dune", line 1, characters 0-54:
  1 | (coq.theory
  2 |  (name B)
  3 |  (package Simple)
  4 |  (theories A))
  Error: No rule found for "lib B/A/a3.vo"
  File "lib B/dune", line 1, characters 0-54:
  1 | (coq.theory
  2 |  (name B)
  3 |  (package Simple)
  4 |  (theories A))
  Error: No rule found for "lib\\"
  [1]

We inspect the contents of the build directory.

  $ ls _build/install/default/lib/coq/user-contrib/A/a1.vo
  _build/install/default/lib/coq/user-contrib/A/a1.vo
  $ ls "_build/default/lib A/a1.vo"
  _build/default/lib A/a1.vo
  $ ls _build/install/default/lib/coq/user-contrib/B/b.vo
  ls: cannot access '_build/install/default/lib/coq/user-contrib/B/b.vo': No such file or directory
  [2]
  $ ls "_build/default/lib B/b.vo"
  ls: cannot access '_build/default/lib B/b.vo': No such file or directory
  [2]
