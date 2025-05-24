Utop will load libs recursively:

  $ touch empty
  $ echo 'open Stdlib;; print_endline "success";; exit 0;;' | dune utop . -- -stdin -init empty
  success


The message where the library path does not exist is different:

  $ dune utop does-not-exist . -- -init ""
  Error: cannot find directory: does-not-exist
  [1]
