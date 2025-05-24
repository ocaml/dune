By default, dune utop tries to make a toplevel for the current directory:

  $ touch empty
  $ echo 'open Stdlib;; print_endline "success";; exit 0;;' | dune utop . -- -stdin -init empty
  success
