Test that when `(stdlib no)` is provided, the standard library is not bound to `Coq`
and the prelude is not imported

  $ dune build --display=short foo.vo
        coqdep foo.v.d
  *** Warning: in file foo.v, library Prelude is required from root Coq and has not been found in the loadpath!
          coqc foo.{glob,vo} (exit 1)
  File "./foo.v", line 1, characters 0-32:
  Error: Cannot find a physical path bound to logical path
  Prelude with prefix Coq.
  
  [1]

  $ dune build --display=short bar.vo
        coqdep bar.v.d
          coqc bar.{glob,vo} (exit 1)
  File "./bar.v", line 1, characters 20-23:
  Error: The reference nat was not found in the current environment.
  
  [1]
