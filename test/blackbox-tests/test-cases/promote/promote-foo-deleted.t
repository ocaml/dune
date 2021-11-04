Test that [promote-foo] syntax is deleted in Dune 3.0 with good error messages.

First, check that the syntax still works with Dune 2.9.

  $ echo "(lang dune 2.9)" > dune-project
  $ cat >dune <<EOF
  > (rule
  >  (targets promoted)
  >  (action (with-stdout-to promoted (echo "Hello, world!")))
  >  (mode (promote-into subdir)))
  > (rule
  >  (targets promoted-until-clean)
  >  (action (with-stdout-to promoted-until-clean (echo "Hello again!")))
  >  (mode promote-until-clean))
  > EOF

  $ mkdir subdir
  $ dune build promoted promoted-until-clean
  $ cat subdir/promoted
  Hello, world!
  $ cat promoted-until-clean
  Hello again!
  $ dune clean
  $ cat subdir/promoted
  Hello, world!
  $ cat promoted-until-clean
  cat: promoted-until-clean: No such file or directory
  [1]

Now switch to Dune 3.0.

  $ echo "(lang dune 3.0)" > dune-project
  $ dune build promoted
  File "dune", line 4, characters 7-28:
  4 |  (mode (promote-into subdir)))
             ^^^^^^^^^^^^^^^^^^^^^
  Error: 'promote-into' was deleted in version 3.0 of the dune language. Use
  the (promote (into <dir>)) syntax instead.
  [1]

  $ cat >dune <<EOF
  > (rule
  >  (targets promoted)
  >  (action (with-stdout-to promoted (echo "Hello again!")))
  >  (mode (promote-until-clean-into subdir)))
  > EOF
  $ dune build promoted
  File "dune", line 4, characters 7-40:
  4 |  (mode (promote-until-clean-into subdir)))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'promote-until-clean-into' was deleted in version 3.0 of the dune
  language. Use the (promote (until-clean) (into <dir>)) syntax instead.
  [1]

  $ cat >dune <<EOF
  > (rule
  >  (targets promoted)
  >  (action (with-stdout-to promoted (echo "Hello again!")))
  >  (mode promote-until-clean))
  > EOF
  $ dune build promoted
  File "dune", line 4, characters 7-26:
  4 |  (mode promote-until-clean))
             ^^^^^^^^^^^^^^^^^^^
  Error: 'promote-until-clean' was deleted in version 3.0 of the dune language.
  Use the (promote (until-clean)) syntax instead.
  [1]

Test that the hint works.

  $ cat >dune <<EOF
  > (rule
  >  (targets promoted)
  >  (action (with-stdout-to promoted (echo "Hello again!")))
  >  (mode (promote (until-clean))))
  > EOF

  $ dune build promoted
  $ cat promoted
  Hello again!
  $ dune clean
  $ cat promoted
  cat: promoted: No such file or directory
  [1]
