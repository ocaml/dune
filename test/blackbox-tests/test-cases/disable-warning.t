Demonstrate dune's system for enabling/disabling warnings

  $ cat >dune-project <<EOF
  > (lang dune 1.5)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (no_keep_locs))
  > EOF

  $ dune build
  File "dune", line 3, characters 1-15:
  3 |  (no_keep_locs))
       ^^^^^^^^^^^^^^
  Warning: no_keep_locs is a no-op. Please delete it.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (no_keep_locs disabled))

Now we demonstrate how this warning can be deleted:

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (warnings
  >  (no_keep_locs disabled))
  > EOF

  $ dune build
