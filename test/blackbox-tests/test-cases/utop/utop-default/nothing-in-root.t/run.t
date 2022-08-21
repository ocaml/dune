Utop will load libs recursively:

  $ echo 'exit 0;;' | dune utop . -- -init "" | grep -v 'version'
  Enter #help;; for help.
  
  Init file not found: "".
  # 


The message where the library path does not exist is different:

  $ dune utop does-not-exist . -- -init ""
  Error: cannot find directory: does-not-exist
  [1]
