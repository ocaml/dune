Here we test the features of the `dune runtest` command. 

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > EOF

  $ cat > mytest.t <<EOF
  >   $ echo "Hello, world!"
  > "Goodbye, world!"
  > EOF
  $ mkdir -p tests/myothertest.t
  $ echo 'Hello, world!' > tests/myothertest.t/hello.world
  $ cat > tests/myothertest.t/run.t <<EOF
  >   $ cat hello.world
  > "Goodbye, world!"
  > EOF
  $ cat > tests/filetest.t <<EOF
  >   $ echo "Hello, world!"
  > "Goodbye, world!"
  > EOF


This should work:

  $ dune test tests/myothertest.t
  File "tests/myothertest.t/run.t", line 1, characters 0-0:
  Error: Files _build/default/tests/myothertest.t/run.t and
  _build/default/tests/myothertest.t/run.t.corrected differ.
  [1]

There's no diff produced because the test passes

  $ dune promotion diff tests/myothertest.t/run.t

This should not work

  $ dune test myotherttest.t
  Error: "myotherttest.t" does not match any known test.
  [1]

This is a bug. Running the test this way does not correctly include the
dependencies.

  $ dune test tests/myothertest.t/run.t
  File "tests/myothertest.t/run.t", line 1, characters 0-0:
  Error: Files _build/default/tests/myothertest.t/run.t and
  _build/default/tests/myothertest.t/run.t.corrected differ.
  [1]

  $ dune promotion diff tests/myothertest.t/run.t

  $ cat _build/.promotion-staging/tests/myothertest.t/run.t
    $ cat hello.world
    cat: hello.world: No such file or directory
    [1]
  "Goodbye, world!"

Passing no arguments to $ dune runtest should be equivalent to $ dune build
@runtest.

  $ dune test 2>&1 | grep "^File"
  File "mytest.t", line 1, characters 0-0:
  File "tests/filetest.t", line 1, characters 0-0:
  File "tests/myothertest.t/run.t", line 1, characters 0-0:

Passing the name of a test should only run that test.

  $ dune test mytest.t 2>&1 | grep "^File"
  File "mytest.t", line 1, characters 0-0:
  $ dune test tests/myothertest.t 2>&1 | grep "^File"
  File "tests/myothertest.t/run.t", line 1, characters 0-0:

Passing a directory should run all the tests in that directory (recursively).

- The current working directory:
  $ dune test . 2>&1 | grep "^File"
  File "mytest.t", line 1, characters 0-0:
  File "tests/filetest.t", line 1, characters 0-0:
  File "tests/myothertest.t/run.t", line 1, characters 0-0:

- The tests/ subdirectory:
  $ dune test tests/ 2>&1 | grep "^File"
  File "tests/filetest.t", line 1, characters 0-0:
  File "tests/myothertest.t/run.t", line 1, characters 0-0:

- We can also build in _build/ directories:
  $ dune test _build/default 2>&1 | grep "^File"
  File "mytest.t", line 1, characters 0-0:
  File "tests/filetest.t", line 1, characters 0-0:
  File "tests/myothertest.t/run.t", line 1, characters 0-0:
  $ dune test _build/default/tests 2>&1 | grep "^File"
  File "tests/filetest.t", line 1, characters 0-0:
  File "tests/myothertest.t/run.t", line 1, characters 0-0:

Here we test some error cases a user may encounter and make sure the error
messages are informative enough.

- Giving a path outside the workspace gives an informative error:
  $ dune test ..
  Error: @@ on the command line must be followed by a relative path
  [1]
- Giving a nonexistent path gives an informative error:
  $ dune test nonexistent
  Error: "nonexistent" does not match any known test.
  [1]
  $ dune test tests/non
  Error: "tests/non" does not match any known test.
  [1]
- Passing the _build directory on its own is an error.
  $ dune test _build
  Error: This path is internal to dune: _build
  [1]
- Typos are caught and aided with hints:
  $ dune test mytest1.t
  Error: "mytest1.t" does not match any known test.
  Hint: did you mean mytest.t?
  [1]
- Running a non-test file should give a suitable error message
  $ dune test dune-project
  Error: "dune-project" does not match any known test.
  [1]
- Typos of diretories are also caught and aided:
  $ dune test tests/myothertest1.t
  Error: "tests/myothertest1.t" does not match any known test.
  Hint: did you mean tests/myothertest.t?
  [1]
  $ dune test testt/
  Error: "testt" does not match any known test.
  Hint: did you mean tests?
  [1]
- Note that this doesn't handle the case where the path is mostly correct but
the directory is mispelled.
  $ dune test testss/myothertest.t
  Error: "testss/myothertest.t" does not match any known test.
  [1]
