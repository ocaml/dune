Test for --swallow-stdout-on-success
====================================

  $ export BUILD_PATH_PREFIX_MAP="sh=$(which sh):$BUILD_PATH_PREFIX_MAP"

  $ echo '(lang dune 3.0)' > dune-project

  $ cat > dune <<EOF
  > (rule
  >  (alias default)
  >  (action (system "echo 'Hello, world!'")))
  > EOF

By default, stdout is always printed:

  $ dune build
            sh alias default
  Hello, world!

With the option, stdout is swallowed when the action succeeds:

  $ dune clean
  $ dune build --swallow-stdout-on-success

Now with an action that fails:

  $ cat > dune <<EOF
  > (rule
  >  (alias default)
  >  (action (system "echo 'Hello, world!'; exit 1")))
  > EOF

It is always printed in case of error:

  $ dune build
  File "dune", line 1, characters 0-73:
  1 | (rule
  2 |  (alias default)
  3 |  (action (system "echo 'Hello, world!'; exit 1")))
            sh alias default (exit 1)
  (cd _build/default && sh -c 'echo '\''Hello, world!'\''; exit 1')
  Hello, world!
  [1]

  $ dune clean
  $ dune build --swallow-stdout-on-success
  File "dune", line 1, characters 0-73:
  1 | (rule
  2 |  (alias default)
  3 |  (action (system "echo 'Hello, world!'; exit 1")))
            sh alias default (exit 1)
  (cd _build/default && sh -c 'echo '\''Hello, world!'\''; exit 1')
  Hello, world!
  [1]

With compound actions
---------------------

At the moment, the behavior is a bit odd. We swallow the stdout of the
first command but not the second:


  $ cat > dune <<EOF
  > (rule
  >  (alias default)
  >  (action
  >   (progn
  >    (system "echo 1")
  >    (system "echo 2; exit 1"))))
  > EOF

  $ dune build --swallow-stdout-on-success
  File "dune", line 1, characters 0-93:
  1 | (rule
  2 |  (alias default)
  3 |  (action
  4 |   (progn
  5 |    (system "echo 1")
  6 |    (system "echo 2; exit 1"))))
            sh alias default (exit 1)
  (cd _build/default && sh -c 'echo 2; exit 1')
  2
  [1]

With builtin actions
--------------------

We currently never swallow the output of builtin actions such as
`echo`, which is odd:

  $ cat > dune <<EOF
  > (rule
  >  (alias default)
  >  (action (echo "Hello, world!\n")))
  > EOF

  $ dune build --swallow-stdout-on-success
  Hello, world!
