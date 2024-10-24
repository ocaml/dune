Here we test the features of the `dune runtest` command. 

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > EOF

  $ cat > mytest.t <<EOF
  >   $ echo "Hello, world!"
  > "Goodbye, world!"
  > EOF
  $ mkdir -p tests/myothertest.t
  $ cat > tests/myothertest.t/run.t <<EOF
  >   $ echo "Hello, world!"
  > "Goodbye, world!"
  > EOF

Passing no arguments to `dune runtest` should be equivalent to `dune build
@runtest`.

  $ dune test 2>&1 | grep "^File"
  File "mytest.t", line 1, characters 0-0:
  File "tests/myothertest.t/run.t", line 1, characters 0-0:

Passing the name of a test should only run that test.
Currently, this is not the case.

  $ dune test mytest
  Error: Don't know about directory mytest specified on the command line!
  [1]
  $ dune test mytest.t
  Error: Don't know about directory mytest.t specified on the command line!
  [1]
  $ dune test tests/myothertest
  Error: Don't know about directory tests/myothertest specified on the command
  line!
  [1]
  $ dune test tests/myothertest.t

Passing a directory should run all the tests in that directory (recursively).

The current working directory:
  $ dune test . 2>&1 | grep "^File"
  File "mytest.t", line 1, characters 0-0:
  File "tests/myothertest.t/run.t", line 1, characters 0-0:

The tests/ subdirectory:
  $ dune test tests/ 2>&1 | grep "^File"
  File "tests/myothertest.t/run.t", line 1, characters 0-0:
