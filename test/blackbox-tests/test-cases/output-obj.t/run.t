  $ dune build @all
  $ dune build @runtest 2>&1 | dune_cmd sanitize
        static alias runtest
  OK: ./static.exe
        static alias runtest
  OK: ./static.bc
       dynamic alias runtest
  OK: ./dynamic.exe ./test.bc$ext_dll
       dynamic alias runtest
  OK: ./dynamic.exe ./test$ext_dll
#        static alias runtest
#  OK: ./static.bc.c.exe
