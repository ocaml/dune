The server resolves dependencies from the launch directory, but the client
currently reads relative to its changed working directory.

  $ cat > dune-project <<'EOF'
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF
  $ cp bin/foo.exe .
  $ cat > dune <<'EOF'
  > (rule (target input) (action (write-file input anchored)))
  > (rule
  >  (target cwd-output)
  >  (action (with-stdout-to cwd-output (dynamic-run ./foo.exe chdir))))
  > EOF
  $ dune build cwd-output
  File "dune", lines 2-4, characters 0-95:
  2 | (rule
  3 |  (target cwd-output)
  4 |  (action (with-stdout-to cwd-output (dynamic-run ./foo.exe chdir))))
  read_file: open(input): No such file or directory
  [1]
  $ cat _build/default/input
  anchored
