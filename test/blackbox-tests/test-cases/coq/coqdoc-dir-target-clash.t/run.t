We try to build the documentation but there will be a clash between the
directory targets.
  $ dune build @check
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "dune", lines 1-7, characters 0-120:
  1 | (rule
  2 |  (targets
  3 |   (dir base.html))
  4 |  (action
  5 |   (progn
  6 |    (run mkdir base.html)
  7 |    (run touch base.html/base.base.html))))
  Error: The following both define the same directory target:
  _build/default/base.html
  - dune:1
  - dune:17
  [1]
