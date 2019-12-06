  $ dune build @all
  $ dune build @runtest
        static alias runtest
  OK: ./static.exe
       dynamic alias runtest
  OK: ./dynamic.exe ./test$ext_dll
        static alias runtest
  OK: ./static.bc
       dynamic alias runtest
  OK: ./dynamic.exe ./test.bc.so
#        static alias runtest
#  OK: ./static.bc.c.exe
