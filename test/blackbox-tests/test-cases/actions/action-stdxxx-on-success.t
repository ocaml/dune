Test for --action-stdxxx-on-success
====================================

  $ export BUILD_PATH_PREFIX_MAP="sh=$(command -v sh):$BUILD_PATH_PREFIX_MAP"

  $ echo '(lang dune 3.0)' > dune-project

  $ cat > dune <<EOF
  > (rule
  >  (alias default)
  >  (action (system "echo 'Hello, world!'")))
  > 
  > (rule
  >  (alias default)
  >  (action (system "echo 'Something went wrong!' >&2")))
  > 
  > (rule
  >  (alias both-stdout-and-stderr-output)
  >  (action (system "echo stdout; echo stderr >&2")))
  > EOF

By default, stdout and stderr are always printed:

  $ dune build
  Hello, world!
  Something went wrong!

swallow tests
-------------

  $ dune clean
  $ dune build --action-stdout-on-success=swallow --action-stderr-on-success=swallow

must-be-empty tests
----------------------

In the two above tests, we ask Dune to enforce that the the stdout
(resp. stderr) of actions is empty via the must-be-empty setting.
Since the first rule has a non-empty stdout and the second has a
non-empty stderr, we observe that in each case the build fails
printing the output of the action that had a non-empty output.

  $ dune clean
  $ dune build --action-stdout-on-success=must-be-empty
  Something went wrong!
  File "dune", line 1, characters 0-65:
  1 | (rule
  2 |  (alias default)
  3 |  (action (system "echo 'Hello, world!'")))
  Hello, world!
  [1]

  $ dune clean
  $ dune build --action-stderr-on-success=must-be-empty
  Hello, world!
  File "dune", line 5, characters 0-77:
  5 | (rule
  6 |  (alias default)
  7 |  (action (system "echo 'Something went wrong!' >&2")))
  Something went wrong!
  [1]

Same but with output on both stdout and stderr:

  $ dune clean
  $ dune build @both-stdout-and-stderr-output \
  >    --action-stdout-on-success=must-be-empty \
  >    --action-stderr-on-success=must-be-empty
  File "dune", line 9, characters 0-95:
   9 | (rule
  10 |  (alias both-stdout-and-stderr-output)
  11 |  (action (system "echo stdout; echo stderr >&2")))
  stdout
  stderr
  [1]


Incremental builds
------------------

Dune handles --action-stdxxx-on-success in such a way that if
changing the status of one of the two option changes what is printed
to the terminal, then the action is re-executed.

  $ dune clean
  $ dune build \
  >   --action-stdout-on-success=swallow \
  >   --action-stderr-on-success=swallow

For instance, if we previously swallowed stdout/stderr and stop doing
it, actions that printed something to stdout or stderr are
re-executed:

  $ dune build
  Hello, world!
  Something went wrong!

However, we currently re-execute too much. In particular, we
re-execute actions whose outcome is not affected by the change:

  $ cat > dune <<EOF
  > (rule
  >  (alias default)
  >  (action (system "echo a.stdout; echo a.stderr >&2")))
  > 
  > (rule
  >  (alias default)
  >  (action (system "echo b.stderr >&2")))
  > EOF

  $ dune clean
  $ dune build --action-stdout-on-success=swallow
  a.stderr
  b.stderr

You can observe in the below call that both actions are being
re-executed:

  $ dune build
  a.stdout
  a.stderr
  b.stderr

However, re-executing the second action was not necessary given that
its stdout was empty. Dune could have recorded the fact that the
second action had an empty stdout and so was unaffected by the status
of --action-stdout-on-success. Dune could also cache the
stdout/stderr of actions across builds and only re-print them rather
than re-execute actions entirely.

In case of errors
-----------------

In case of errors, we print everything no matter what.

  $ cat > dune <<EOF
  > (rule
  >  (alias default)
  >  (action (system "echo 'Hello, world!'; exit 1")))
  > EOF

  $ dune build
  File "dune", line 1, characters 0-73:
  1 | (rule
  2 |  (alias default)
  3 |  (action (system "echo 'Hello, world!'; exit 1")))
  Hello, world!
  [1]

  $ dune clean
  $ dune build --action-stdout=swallow
  File "dune", line 1, characters 0-73:
  1 | (rule
  2 |  (alias default)
  3 |  (action (system "echo 'Hello, world!'; exit 1")))
  Hello, world!
  [1]

  $ dune clean
  $ dune build --action-stdout=must-be-empty
  File "dune", line 1, characters 0-73:
  1 | (rule
  2 |  (alias default)
  3 |  (action (system "echo 'Hello, world!'; exit 1")))
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

  $ dune build --action-stdout-on-success=swallow
  File "dune", line 1, characters 0-93:
  1 | (rule
  2 |  (alias default)
  3 |  (action
  4 |   (progn
  5 |    (system "echo 1")
  6 |    (system "echo 2; exit 1"))))
  2
  [1]

For must-be-empty, if two programs print something without failing we
stop at the first program. That's not terrible, but it would seem
better if we stop at the end of the whole action.

  $ cat > dune <<EOF
  > (rule
  >  (alias default)
  >  (action
  >   (progn
  >    (system "echo 1")
  >    (system "echo 2"))))
  > EOF

  $ dune build --action-stdout-on-success=must-be-empty
  File "dune", line 1, characters 0-85:
  1 | (rule
  2 |  (alias default)
  3 |  (action
  4 |   (progn
  5 |    (system "echo 1")
  6 |    (system "echo 2"))))
  1
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

  $ dune build --action-stdout-on-success=swallow
  Hello, world!
