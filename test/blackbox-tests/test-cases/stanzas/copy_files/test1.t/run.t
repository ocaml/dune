Test that (copy_files ...) works

  $ dune build test.exe .merlin-conf/lib-foo .merlin-conf/exe-test
  $ dune build @bar-source
  #line 1 "include/bar.h"
  int foo () {return 42;}
