Here we test what happens when coq --config or --print-version fails in an unexpected way
and how dune reacts to this failure.

First we create a wrapper around coqc so we can make it fail. It should only fail on
--config and --print-version.
  $ mkdir bin
  $ cat > bin/coqc <<'EOF'
  > #!/bin/sh
  > if    [ $1 = --config ]        && [ -n "$FAIL_CONFIG" ]; then
  >   echo "coqc --config has failed for some reason" >&2
  >   exit 1
  > elif  [ $1 = --print-version ] && [ -n "$FAIL_VERSION" ]; then
  >   echo "coqc --print-version has failed for some reason" >&2
  >   exit 1
  > fi
  > EOF
  > echo "$(which coqc) \$@" >> bin/coqc
  > chmod +x bin/coqc

  $ export PATH=$PWD/bin:$PATH

To make sure these are working correctly we test them.

These should succeed.
  $ coqc --print-version > /dev/null
  $ coqc --config > /dev/null
These should fail.
  $ FAIL_VERSION=1 \
  > coqc --print-version 2> /dev/null
  [1]
  $ FAIL_CONFIG=1 \
  > coqc --config 2> /dev/null
  [1]

Now we create a simple project that uses this coqc wrapper, should
fail when the stdlib cannot be determined

  $ cat > dune <<EOF
  > (coq.theory
  >  (flags -noinit)
  >  (name foo))
  > 
  > (rule
  >  (deps
  >   (env_var FAIL_VERSION)
  >   (env_var FAIL_CONFIG))
  >  (action
  >   (write-file a.v "")))
  > EOF

Should fail: first warning that installed theories are being skipped due to the
failure, then, as the library requires the stdlib, it fails:
  $ FAIL_CONFIG=1 \
  > dune build
  Warning: Skipping installed theories due to 'coqc --config' failure:
  - $TESTCASE_ROOT/bin/coqc --config failed with exit code 1.
  Hint: Try running 'coqc --config' manually to see the error.
  Couldn't find Coq standard library, and theory is not using (stdlib no)
  -> required by _build/default/.foo.theory.d
  -> required by alias all
  -> required by alias default
  [1]

Here we build a simple Coq project. Neither a failing --config or --print-version should
block this as it doesn't depend on the stdlib.

  $ cat > dune <<EOF
  > (coq.theory
  >  (flags -noinit)
  >  (name foo)
  >  (stdlib no))
  > 
  > (rule
  >  (deps
  >   (env_var FAIL_VERSION)
  >   (env_var FAIL_CONFIG))
  >  (action
  >   (write-file a.v "")))
  > EOF

Should succeed, warning that installed theories are being skipped due to the
failure (c.f. #8958):
  $ FAIL_CONFIG=1 \
  > dune build
  Warning: Skipping installed theories due to 'coqc --config' failure:
  - $TESTCASE_ROOT/bin/coqc --config failed with exit code 1.
  Hint: Try running 'coqc --config' manually to see the error.

  $ FAIL_VERSION=1 \
  > dune build

  $ dune build

Here we query the version of Coq. Due to the expansion of %{coq:_} macros we need coq
--config. A failing --print-version or --config will block this value from being realised.

  $ cat > dune <<EOF
  > (rule
  >  (alias version)
  >  (action
  >   (echo %{coq:version})))
  > EOF

Fails for now, but could be improved in the future.
  $ FAIL_CONFIG=1 \
  > dune build @version
  File "dune", line 4, characters 8-22:
  4 |   (echo %{coq:version})))
              ^^^^^^^^^^^^^^
  Error: Could not expand %{coq:version} as running coqc --config failed.
  $TESTCASE_ROOT/bin/coqc --config failed with exit code 1.
  Hint: coqc --config requires the coq-stdlib package in order to function
  properly.
  [1]

Should fail.
  $ FAIL_VERSION=1 \
  > dune build @version
  Error: Could not parse coqc version: 
  $TESTCASE_ROOT/bin/coqc --print-version failed with exit code 1.
  -> required by %{coq:version} at dune:4
  -> required by alias version in dune:1
  [1]

Here we query the config. A failing --config will block this value from being realised
however a failing --print-version will not.

  $ cat > dune <<EOF
  > (rule
  >  (alias config)
  >  (action
  >   (echo %{coq:coqlib})))
  > EOF

Should fail.
  $ export coqlib="$(coqc -config | grep COQLIB | sed 's/COQLIB=//')"
  $ FAIL_CONFIG=1 \
  > dune build @config 
  File "dune", line 4, characters 8-21:
  4 |   (echo %{coq:coqlib})))
              ^^^^^^^^^^^^^
  Error: Could not expand %{coq:coqlib} as running coqc --config failed.
  $TESTCASE_ROOT/bin/coqc --config failed with exit code 1.
  Hint: coqc --config requires the coq-stdlib package in order to function
  properly.
  [1]

Should succeed.
  $ FAIL_VERSION=1 \
  > dune build @config | sed "s,$coqlib,COQLIB," > /dev/null

Should succeed.
  $ dune build @config | sed "s,$coqlib,COQLIB," > /dev/null
