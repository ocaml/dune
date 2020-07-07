Test that (copy_files ...) works

  $ dune build --root test1 test.exe .merlin
  Entering directory 'test1'
  $ dune build --root test1 @bar-source
  Entering directory 'test1'
  #line 1 "include/bar.h"
  int foo () {return 42;}
  $ dune build --root test2 @foo/cat
  Entering directory 'test2'
  # 1 "dummy.txt"
  hello
