This test checks that executable that uses 'dynamic-run'
and requires dependency that can not be build fails.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ cp ./bin/foo.exe ./

  $ dune runtest 2>&1 | awk '/Internal error/,/unable to serialize/'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("unable to serialize exception",
