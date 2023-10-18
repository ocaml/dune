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

Now we create a simple project that uses this coqc wrapper.

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

Here we build a simple Coq project. Neither a failing --config or --print-version should
block this.

Should succeed, but should warn that installed theories are being skipped due to the
failure.
  $ FAIL_CONFIG=1 \
  > dune build
  coqc --config has failed for some reason
  Error: Error while running
  $TESTCASE_ROOT/bin/coqc
  --config
  Exit code: 1
  Output:
  
  [1]

  $ FAIL_VERSION=1 \
  > dune build
  coqc --print-version has failed for some reason
  [1]

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
  coqc --config has failed for some reason
  Error: Error while running
  $TESTCASE_ROOT/bin/coqc
  --config
  Exit code: 1
  Output:
  
  -> required by %{coq:version} at dune:4
  -> required by alias version in dune:1
  [1]

Should fail.
  $ FAIL_VERSION=1 \
  > dune build @version
  coqc --print-version has failed for some reason
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
  coqc --config has failed for some reason
  Error: Error while running
  $TESTCASE_ROOT/bin/coqc
  --config
  Exit code: 1
  Output:
  
  -> required by %{coq:coqlib} at dune:4
  -> required by alias config in dune:1
  [1]

Should succeed.
  $ FAIL_VERSION=1 \
  > dune build @config | sed "s,$coqlib,COQLIB," > /dev/null
  coqc --print-version has failed for some reason
  -> required by %{coq:coqlib} at dune:4
  -> required by alias config in dune:1

Should succeed.
  $ dune build @config | sed "s,$coqlib,COQLIB," > /dev/null
