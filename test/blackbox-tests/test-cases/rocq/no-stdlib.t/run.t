Test that when `(stdlib no)` is provided, the standard library is not bound to `Coq`
and the prelude is not imported; we expect the below two tests to fail.

  $ dune build --display=short foo.vo
        coqdep .basic.theory.d
  Warning: in file foo.v, library Prelude is required
           from root Corelib and has not been found in the loadpath!
           [module-not-found,filesystem,default]
          coqc foo.{glob,vo} (exit 1)
  File "./foo.v", line 1, characters 28-35:
  Error: Cannot find a physical path bound to logical path
  Prelude with prefix Corelib.
  
  [1]

  $ dune build --display=short bar.vo
          coqc bar.{glob,vo} (exit 1)
  File "./bar.v", line 1, characters 20-23:
  Error: The reference nat was not found in the current environment.
  
  [1]
