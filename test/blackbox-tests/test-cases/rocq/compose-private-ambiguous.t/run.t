We have two theories, A and A_vendored both called A. We test which one a
private plugin B and the public package C will pick up.

B will pick up the private one:
  $ dune build B
  message = "I am the private A "
       : string

C picks up the private one too:
  $ dune build C
  File "C/dune", line 4, characters 11-12:
  4 |  (theories A))
                 ^
  Theory "A" is private, it cannot be a dependency of a public theory. You need
  to associate "A" to a package.
  [1]
