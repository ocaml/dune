Preventing regression about the RPC promotion which didn't print a warning
where the normal promotion did. They should now be the same.

  $ echo '(lang dune 3.20)' > dune-project

Something to promote for A
  $ echo "  $ echo hello" > a.t

Nothing to promote for B
  $ cat > b.t << EOF
  >   $ echo hello
  >   hello
  > EOF

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

Now A is in the promotion database.
  $ dune build @a
  File "a.t", line 1, characters 0-0:
  Error: Files _build/default/a.t and _build/default/a.t.corrected differ.
  Error: Build failed with 1 error.
  [1]

This should be a success for A (and print nothing), and a warning for B.
  $ dune promote a.t b.t
  Warning: Nothing to promote for b.t.
  Warning: Build completed with 1 warning.

  $ stop_dune
  File "a.t", line 1, characters 0-0:
  Error: Files _build/default/a.t and _build/default/a.t.corrected differ.
  Had 1 error, waiting for filesystem changes...
  Promoting _build/default/a.t.corrected to a.t.
