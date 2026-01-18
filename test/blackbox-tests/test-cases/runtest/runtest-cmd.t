Here we test the features of the `dune runtest` command. 

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > EOF

  $ cat > mytest.t <<EOF
  >   $ echo "Hello, world!"
  >   "Goodbye, world!"
  > EOF
  $ mkdir -p tests/myothertest.t
  $ echo 'Hello, world!' > tests/myothertest.t/hello.world
  $ cat > tests/myothertest.t/run.t <<EOF
  >   $ cat hello.world
  >   "Goodbye, world!"
  > EOF
  $ cat > tests/filetest.t <<EOF
  >   $ echo "Hello, world!"
  >   "Goodbye, world!"
  > EOF

dune runtest should be able to run a specfic test. In this case,
tests/myothertest.t should fail because the expected output is different from
the observed output.

  $ dune test tests/myothertest.t
  File "tests/myothertest.t/run.t", line 1, characters 0-0:
  Error: Files _build/default/tests/myothertest.t/run.t and
  _build/default/tests/myothertest.t/run.t.corrected differ.
  [1]
  $ dune promotion diff tests/myothertest.t/run.t

We use the promotion diff command to check there is a promotion pending. If
there is no promotion it will warn. 

If the user writes the run.t file of a directory test, we should correct it to
be the corresponding directory cram test.

  $ dune test tests/myothertest.t/run.t
  File "tests/myothertest.t/run.t", line 1, characters 0-0:
  Error: Files _build/default/tests/myothertest.t/run.t and
  _build/default/tests/myothertest.t/run.t.corrected differ.
  [1]
  $ dune promotion diff tests/myothertest.t/run.t

We cannot give the name of a cram test in a subdirectory and expect Dune to
find it.

  $ dune test myothertest.t
  Error: "myothertest.t" does not match any known test.
  [1]

  $ dune promotion diff tests/myothertest.t/run.t
  Warning: Nothing to promote for tests/myothertest.t/run.t.

Passing no arguments to $ dune runtest should be equivalent to $ dune build
@runtest.

  $ dune test 2>&1 | grep "^File"
  File "mytest.t", line 1, characters 0-0:
  File "tests/filetest.t", line 1, characters 0-0:
  File "tests/myothertest.t/run.t", line 1, characters 0-0:
  [1]

Passing the name of a test should only run that test.

  $ dune test mytest.t 2>&1 | grep "^File"
  File "mytest.t", line 1, characters 0-0:
  [1]
  $ dune test tests/myothertest.t 2>&1 | grep "^File"
  File "tests/myothertest.t/run.t", line 1, characters 0-0:
  [1]

Passing a directory should run all the tests in that directory (recursively).

- The current working directory:
  $ dune test . 2>&1 | grep "^File"
  File "mytest.t", line 1, characters 0-0:
  File "tests/filetest.t", line 1, characters 0-0:
  File "tests/myothertest.t/run.t", line 1, characters 0-0:
  [1]

- The tests/ subdirectory:
  $ dune test tests/ 2>&1 | grep "^File"
  File "tests/filetest.t", line 1, characters 0-0:
  File "tests/myothertest.t/run.t", line 1, characters 0-0:
  [1]

- We can also build in _build/ directories:
  $ dune test _build/default 2>&1 | grep "^File"
  File "mytest.t", line 1, characters 0-0:
  File "tests/filetest.t", line 1, characters 0-0:
  File "tests/myothertest.t/run.t", line 1, characters 0-0:
  [1]
  $ dune test _build/default/tests 2>&1 | grep "^File"
  File "tests/filetest.t", line 1, characters 0-0:
  File "tests/myothertest.t/run.t", line 1, characters 0-0:
  [1]

- We can build in absolute paths:
  $ dune test $PWD/mytest.t
  File "mytest.t", line 1, characters 0-0:
  Error: Files _build/default/mytest.t and _build/default/mytest.t.corrected
  differ.
  [1]
  $ dune test $PWD/_build/default/mytest.t
  File "mytest.t", line 1, characters 0-0:
  Error: Files _build/default/mytest.t and _build/default/mytest.t.corrected
  differ.
  [1]

Here we test some error cases a user may encounter and make sure the error
messages are informative enough.

- Giving a path outside the workspace gives an informative error:
  $ dune test ..
  Error: path outside the workspace: .. from .
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
- Note that this does not handle the case where the path is mostly correct but
the directory is mispelled.
  $ dune test testss/myothertest.t
  Error: "testss/myothertest.t" does not match any known test.
  [1]
- Private _build/ paths should be rejected.
  $ dune test _build/_private
  Error: This path is internal to dune: _build/_private
  [1]
- Install paths should be rejected.
  $ dune test _build/install/default
  Error: This path is internal to dune: _build/install/default
  [1]
- Absolute paths that are invalid are rejected clearly:
  $ dune test $PWD/..
  Error: path outside the workspace: .. from .
  [1]
  $ dune test /a/b/c/
  Error: This path is outside the workspace: /a/b/c/
  [1]

Here we test behaviour for running tests in specific contexts.

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (context (default))
  > (context (default (name alt)))
  > EOF

- Building a specific test now will run them in both contexts.

  $ dune test mytest.t
  File "mytest.t", line 1, characters 0-0:
  Error: Files _build/alt/mytest.t and _build/alt/mytest.t.corrected differ.
  File "mytest.t", line 1, characters 0-0:
  Error: Files _build/default/mytest.t and _build/default/mytest.t.corrected
  differ.
  [1]

- Building a specific test in a specific build directory will build only in
that context as expected.

  $ dune test _build/alt/mytest.t
  File "mytest.t", line 1, characters 0-0:
  Error: Files _build/alt/mytest.t and _build/alt/mytest.t.corrected differ.
  [1]

