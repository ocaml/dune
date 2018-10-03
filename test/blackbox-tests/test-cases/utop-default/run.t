By default, dune utop tries to make a toplevel for the current directory:

  $ echo 'exit 0;;' | dune utop --root lib-in-root | grep -v 'version'
  Entering directory 'lib-in-root'
  
  # 


Utop will load libs recursively:

  $ echo 'exit 0;;' | dune utop --root nothing-in-root | grep -v 'version'
  Entering directory 'nothing-in-root'
  
  # 


The message where the library path does not exist is different:

  $ dune utop --root nothing-in-root does-not-exist
  Cannot find directory: does-not-exist
  [1]
