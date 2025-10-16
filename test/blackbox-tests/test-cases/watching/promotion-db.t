Demonstrate bug where the RPC promotion doesn't print a warning where the normal promotion does.

  $ . ./helpers.sh

  $ echo '(lang dune 3.20)' > dune-project

Something to promote for a
  $ echo "  $ echo hello" > a.t

Nothing to promote for b
  $ cat > b.t << EOF
  >   $ echo hello
  >   hello
  > EOF

Something to promote for c, but we don't care for it right now.
  $ echo "  $ echo hello" > c.t

Promoting a at this point does nothing, and we print a warning.
  $ dune promote a.t
  Warning: Nothing to promote for a.t.

  $ start_dune
  $ dune rpc ping --wait
  Server appears to be responding normally

This should be a warning for a and b.
  $ dune promote a.t b.t
  Success

Now a is in the promotion database,
  $ build "(alias a)"
  Failure

and c as well.
  $ build "(alias c)"
  Failure

This should be a success for a (and print nothing), and a warning for b.
  $ dune promote a.t b.t
  Success

This is incorrect!
  $ cat a.t
    $ echo hello

  $ cat b.t
    $ echo hello
    hello

C should still be in the database at this point, and promotion should work.
  $ dune promote c.t
  Success

  $ cat c.t
    $ echo hello
    hello

  $ stop_dune
  Warning: Nothing to promote for a.t.
  Warning: Nothing to promote for b.t.
  File "a.t", line 1, characters 0-0:
  Error: Files _build/default/a.t and _build/default/a.t.corrected differ.
  Had 1 error, waiting for filesystem changes...
  File "c.t", line 1, characters 0-0:
  Error: Files _build/default/c.t and _build/default/c.t.corrected differ.
  Had 1 error, waiting for filesystem changes...
  Warning: Nothing to promote for a.t.
  Warning: Nothing to promote for b.t.
  Promoting _build/default/c.t.corrected to c.t.
