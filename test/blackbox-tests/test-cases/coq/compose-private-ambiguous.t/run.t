We have two theories, A and A_vendored both called A. We test which one a
private plugin B and the public package C will pick up.

B will pick up the private one:
  $ dune build B
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  message = "I am the the private A "
       : string

C picks up the private one too:
  $ dune build C
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  File "C/dune", line 4, characters 11-12:
  4 |  (theories A))
                 ^
  Theory "A" is private, it cannot be a dependency of a public theory. You need
  to associate "A" to a package.
  [1]
