The following should succeed.

  $ dune build coqlib/theories/plugin.vo
  Warning: Cannot open directory ../../somelib/src
           [cannot-open-dir,filesystem,default]
  File "./coqlib/theories/plugin.v", line 1, characters 0-41:
  Error:
  Dynlink error: error loading shared library: Dynlink.Error (Dynlink.Cannot_open_dll "Failure(\"$TESTCASE_ROOT/_build/install/default/lib/somelib/somelib.cmxs: cannot open shared object file: No such file or directory\")")
  
  [1]
