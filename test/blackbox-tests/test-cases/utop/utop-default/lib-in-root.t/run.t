By default, dune utop tries to make a toplevel for the current directory:

  $ echo 'exit 0;;' | dune utop . -- -init "" | grep -v 'version'
  Enter #help;; for help.
  
  Init file not found: "".
  # 
