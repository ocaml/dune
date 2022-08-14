Test that (ignore_files ...) works

  $ dune build --root test1 test.exe
  Entering directory 'test1'

  $ dune exec --root test1 ./test.exe
  Entering directory 'test1'
  Hello world!

Test that (ignore_files ...) works with a wildcard pattern

  $ dune build --root test2 test.exe
  Entering directory 'test2'

  $ dune exec --root test2 ./test.exe
  Entering directory 'test2'
  Hello world!

  $ dune build --root test2 test2.exe
  Entering directory 'test2'

  $ dune exec --root test2 ./test2.exe
  Entering directory 'test2'
  Hi :)

Test that (ignore_files ...) works with unmatched wildcard patterns

  $ dune build --root test3 test.exe
  Entering directory 'test3'

  $ dune exec --root test3 ./test.exe
  Entering directory 'test3'
  Hello world!
