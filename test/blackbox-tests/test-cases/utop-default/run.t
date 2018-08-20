By default, dune utop tries to make a toplevel for the current directory:

  $ echo 'exit 0;;' | dune utop --root lib-in-root | grep -v 'version'
  Entering directory 'lib-in-root'
  
  # 

If there is no library there, it displays an error message:

  $ dune utop --root nothing-in-root
  Entering directory 'nothing-in-root'
  No library is defined in .
  [1]

The message where the library path does not exist is different:

  $ dune utop --root nothing-in-root does-not-exist
  Cannot find directory: does-not-exist
  [1]
