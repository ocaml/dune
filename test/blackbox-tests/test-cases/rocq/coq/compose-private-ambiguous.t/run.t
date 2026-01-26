We have two theories, A and A_vendored both called A. We test which one a
private plugin B and the public package C will pick up.

B will pick up the private one:
  $ dune build B
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  message = "I am the private A "
       : string

C picks up the private one too:
  $ dune build C
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  File "C/dune", line 4, characters 11-12:
  4 |  (theories A))
                 ^
  Theory "A" is private, it cannot be a dependency of a public theory. You need
  to associate "A" to a package.
  [1]
