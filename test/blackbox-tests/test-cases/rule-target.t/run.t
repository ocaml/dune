Rules with targets outside the build directory are dot allowed.

  $ echo '(lang dune 1.10)' > dune-project
  $ cat > dune <<EOF
  > (rule (with-stdout-to /abs/path (system "echo toto")))
  > EOF

  $ dune build @all
  File "dune", line 1, characters 22-31:
  1 | (rule (with-stdout-to /abs/path (system "echo toto")))
                            ^^^^^^^^^
  Error: Target /abs/path is outside the build directory. This is not allowed.
  [1]

Rules with targets whose names clash with Dune's internal directories
are not allowed.

  $ cat > dune <<EOF
  > (rule (with-stdout-to .dune (system "echo toto")))
  > EOF

  $ dune build @all
  Error: open: _build/default/.dune: Is a directory
  -> required by _build/default/.dune
  -> required by alias all
  [1]
