Test for --fail-on-non-empty-stderr
===================================

  $ export BUILD_PATH_PREFIX_MAP="sh=$(which sh):$BUILD_PATH_PREFIX_MAP"

  $ echo '(lang dune 3.0)' > dune-project

  $ cat > dune <<EOF
  > (rule
  >  (alias default)
  >  (action (system "echo 'Something went wrong!' >&2")))
  > EOF

By default, an action printing something to stderr but returning 0 is
considered as a success:

  $ dune build
            sh alias default
  Something went wrong!

With the option, the action is considered as failed:

  $ dune clean
  $ dune build --fail-on-non-empty-stderr
  File "dune", line 1, characters 0-77:
  1 | (rule
  2 |  (alias default)
  3 |  (action (system "echo 'Something went wrong!' >&2")))
            sh alias default (exit 0)
  (cd _build/default && sh -c 'echo '\''Something went wrong!'\'' >&2')
  Something went wrong!
  [1]

Incremental builds
------------------

  $ cat > dune <<EOF
  > (rule
  >  (alias default)
  >  (action (system "echo 'Hello, world!'")))
  > EOF

  $ dune build
            sh alias default
  Hello, world!

If we suddently enable the option, we re-run everything, including all
actions that succeed and have to stderr output at all:

  $ dune build --fail-on-non-empty-stderr
            sh alias default
  Hello, world!

It feels like we could do better and only re-run the actions that
previously add a non-empty stderr.

With compound actions
---------------------

At the moment, if two programs print to stderr without failing, we
stop at the first program. That's not terrible, but it would seem
better if we stop at the end of the whole action.

  $ cat > dune <<EOF
  > (rule
  >  (alias default)
  >  (action
  >   (progn
  >    (system "echo 1 >&2")
  >    (system "echo 2 >&2"))))
  > EOF

  $ dune build --fail-on-non-empty-stderr
  File "dune", line 1, characters 0-93:
  1 | (rule
  2 |  (alias default)
  3 |  (action
  4 |   (progn
  5 |    (system "echo 1 >&2")
  6 |    (system "echo 2 >&2"))))
            sh alias default (exit 0)
  (cd _build/default && sh -c 'echo 1 >&2')
  1
  [1]
