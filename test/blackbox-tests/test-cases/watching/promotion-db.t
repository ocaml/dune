Demonstrate bug where the RPC promotion doesn't print a warning where the normal promotion does.

  $ . ./helpers.sh

  $ echo '(lang dune 3.20)' > dune-project

Something to promote for A
  $ echo "  $ echo hello" > a.t

Nothing to promote for B
  $ cat > b.t << EOF
  >   $ echo hello
  >   hello
  > EOF

Something to promote for C, but we don't care for it right now.
  $ echo "  $ echo hello" > c.t

Promoting A at this point does nothing, and we print a warning.
  $ dune promote a.t
  Warning: Nothing to promote for a.t.

  $ start_dune
  $ dune rpc ping --wait
  Server appears to be responding normally

This should be a warning for both A and B.
  $ dune promote a.t b.t
  Warning: Nothing to promote for a.t.
  Warning: Nothing to promote for b.t.
  Warning: Build completed with 2 warnings.

Now A and C are in the promotion database.
  $ dune build @a @c
  File "a.t", line 1, characters 0-0:
  Error: Files _build/default/a.t and _build/default/a.t.corrected differ.
  File "c.t", line 1, characters 0-0:
  Error: Files _build/default/c.t and _build/default/c.t.corrected differ.
  Error: Build failed with 2 errors.
  [1]

This should be a success for A (and print nothing), and a warning for B.
  $ dune promote a.t b.t
  Warning: Nothing to promote for b.t.
  Warning: Build completed with 1 warning.

  $ cat a.t
    $ echo hello
    hello

C should still be in the database at this point, and promotion should work.
  $ dune promote c.t
  Success

  $ cat c.t
    $ echo hello
    hello

  $ stop_dune
  File "a.t", line 1, characters 0-0:
  Error: Files _build/default/a.t and _build/default/a.t.corrected differ.
  File "c.t", line 1, characters 0-0:
  Error: Files _build/default/c.t and _build/default/c.t.corrected differ.
  Had 2 errors, waiting for filesystem changes...
  Promoting _build/default/a.t.corrected to a.t.
  Promoting _build/default/c.t.corrected to c.t.
