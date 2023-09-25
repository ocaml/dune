Check that `(shell ..)` option on `(cram ..)` stanza fail before `(lang dune 3.12)`

  $ echo '(lang dune 3.11)' > dune-project

  $ cat > dune <<EOF
  > (cram (shell :system))
  > EOF

  $ cat > foo.t <<EOF
  >   $ echo "foo from foo.t"
  > EOF

  $ dune runtest -f
  File "dune", line 1, characters 6-21:
  1 | (cram (shell :system))
            ^^^^^^^^^^^^^^^
  Error: 'shell' is only available since version 3.12 of the dune language.
  Please update your dune-project file to have (lang dune 3.12).
  [1]

Check that `(shell ..)` option on `(cram ..)` user action fail before `(lang dune 3.12)`

  $ echo '(lang dune 3.11)' > dune-project

  $ cat > dune <<EOF
  > (rule
  >   (target foo.cram.corrected)
  >   (action (cram foo.cram (shell bash)))
  > )
  > EOF

  $ cat > foo.cram <<EOF
  >   $ echo "foo from foo.cram"
  > EOF

  $ dune runtest -f
  File "dune", line 3, characters 25-37:
  3 |   (action (cram foo.cram (shell bash)))
                               ^^^^^^^^^^^^
  Error: 'shell' is only available since version 3.12 of the dune language.
  Please update your dune-project file to have (lang dune 3.12).
  [1]
