Test that when `(stdlib no)` is provided, the standard library is not bound to `Coq`
and the prelude is not imported; we expect the below two tests to fail.

  $ dune build --display=short foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
        coqdep .basic.theory.d
  *** Warning: in file foo.v, library Prelude is required from root Coq and has not been found in the loadpath!
          coqc foo.{glob,vo} (exit 1)
  File "./foo.v", line 1, characters 0-32:
  Error: Cannot find a physical path bound to logical path
  Prelude with prefix Coq.
  
  [1]

  $ dune build --display=short bar.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
          coqc bar.{glob,vo} (exit 1)
  File "./bar.v", line 1, characters 20-23:
  Error: The reference nat was not found in the current environment.
  
  [1]
