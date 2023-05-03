Test that when `(stdlib no)` is provided, the standard library is not bound to `Coq`
and the prelude is not imported

  $ dune build --display=short foo.vo
        coqdep .basic.theory.d
          coqc foo.{glob,vo}

  $ dune build --display=short bar.vo
          coqc bar.{glob,vo} (exit 1)
  File "./bar.v", line 1, characters 20-23:
  Error: The reference nat was not found in the current environment.
  
  [1]
