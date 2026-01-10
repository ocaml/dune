Testing composition of theories with two private theories and a third public
theory. As expected, the private theories build, but the public theory fails
because a public theory cannot depend on a private theory. 

  $ dune build
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  Hello
       : Set
  File "C/dune", line 4, characters 11-12:
  4 |  (theories B))
                 ^
  Theory "B" is private, it cannot be a dependency of a public theory. You need
  to associate "B" to a package.
  [1]
