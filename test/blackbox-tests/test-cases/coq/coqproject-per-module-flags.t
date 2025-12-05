Testing the _CoqProject generation.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using coq 0.11)
  > EOF

  $ cat > dune <<EOF
  > (coq.theory
  >  (name a)
  >  (modules_flags (foo (:standard -flag)))
  >  (generate_project_file))
  > EOF

  $ touch foo.v

  $ dune build
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  File "dune", line 4, characters 1-24:
  4 |  (generate_project_file))
       ^^^^^^^^^^^^^^^^^^^^^^^
  Error: (generate_project_file) is not compatible with (modules_flags ...)
  [1]
