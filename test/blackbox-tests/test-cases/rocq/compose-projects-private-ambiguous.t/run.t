Testing ambiguous composition of private and public theories

1. theory `A` is public belonging to `A/dune-project`
2. theory `B` is public belong to `B/dune-project`.
2. theory `C` is public belonging to `C/dune-project` and depends on `B`.
3. theory `A` is private belonging to `C/dune-project` and depends on `C`.

Currently Dune does not detect this issue, so passes invalid flags to coqdep
which complains.

  $ dune build B
  Warning: in file b.v, library a is required
           from root A and has not been found in the loadpath!
           [module-not-found,filesystem,default]
  File "./B/b.v", line 2, characters 22-23:
  Error: Cannot find a physical path bound to logical path a with prefix A.
  
  [1]

  $ dune build C
  Warning: in file c.v, library a is required
           from root A and has not been found in the loadpath!
           [module-not-found,filesystem,default]
  Warning: in file a.v, library A is required
           from root C and has not been found in the loadpath!
           [module-not-found,filesystem,default]
  File "./C/c.v", line 2, characters 22-23:
  Error: Cannot find a physical path bound to logical path a with prefix A.
  
  File "C/A_vendored/dune", lines 1-3, characters 0-37:
  1 | (rocq.theory
  2 |  (name A)
  3 |  (theories C))
  Warning:
  $TESTCASE_ROOT/_build/default/C/A_vendored
  was previously bound to C.A_vendored; it is remapped to A
  [overriding-logical-loadpath,filesystem,default]
  File "./C/A_vendored/a.v", line 1, characters 22-23:
  Error: Unable to locate library
  A with prefix C (while searching for a .vos file).
  
  [1]

  $ dune build A

  $ dune build C/A_vendored
  File "C/A_vendored/dune", lines 1-3, characters 0-37:
  1 | (rocq.theory
  2 |  (name A)
  3 |  (theories C))
  Warning:
  $TESTCASE_ROOT/_build/default/C/A_vendored
  was previously bound to C.A_vendored; it is remapped to A
  [overriding-logical-loadpath,filesystem,default]
  File "./C/A_vendored/a.v", line 1, characters 22-23:
  Error: Unable to locate library
  A with prefix C (while searching for a .vos file).
  
  [1]
