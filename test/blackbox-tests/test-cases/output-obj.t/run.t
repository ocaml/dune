  $ dune build @all
  $ dune build @runtest 2>&1 | dune_cmd sanitize
  OK: ./static.exe
  OK: ./static.bc
  OK: ./dynamic.exe ./test.bc$ext_dll
  OK: ./dynamic.exe ./test$ext_dll
#        static alias runtest
#  OK: ./static.bc.c.exe
