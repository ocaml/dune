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
  $ cat > tests/filetest.t <<EOF
  >   $ echo "Hello, world!"
  > "Goodbye, world!"
  > EOF

Passing no arguments to `dune runtest` should be equivalent to `dune build
@runtest`.

  $ dune test 2>&1 | grep "^File"
  File "mytest.t", line 1, characters 0-0:
  File "tests/filetest.t", line 1, characters 0-0:
  File "tests/myothertest.t/run.t", line 1, characters 0-0:

Passing the name of a test should only run that test.

  $ dune test mytest 2>&1 | grep "^File"
  File "mytest.t", line 1, characters 0-0:
  $ dune test mytest.t 2>&1 | grep "^File"
  File "mytest.t", line 1, characters 0-0:
  $ dune test tests/myothertest 2>&1 | grep "^File"
  File "tests/myothertest.t/run.t", line 1, characters 0-0:
  $ dune test tests/myothertest.t 2>&1 | grep "^File"
  File "tests/myothertest.t/run.t", line 1, characters 0-0:

We don't have to fully qualify tests in subdirectories.
  $ dune test myothertest 2>&1 | grep "^File"
  File "tests/myothertest.t/run.t", line 1, characters 0-0:
  $ dune test filetest 2>&1 | grep "^File"
  File "tests/filetest.t", line 1, characters 0-0:
  $ dune test filetest.t 2>&1 | grep "^File"
  File "tests/filetest.t", line 1, characters 0-0:

Passing a directory should run all the tests in that directory (recursively).

The current working directory:
  $ dune test . 2>&1 | grep "^File"
  File "mytest.t", line 1, characters 0-0:
  File "tests/filetest.t", line 1, characters 0-0:
  File "tests/myothertest.t/run.t", line 1, characters 0-0:

The tests/ subdirectory:
  $ dune test tests/ 2>&1 | grep "^File"
  File "tests/filetest.t", line 1, characters 0-0:
  File "tests/myothertest.t/run.t", line 1, characters 0-0:
