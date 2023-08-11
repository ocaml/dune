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

  $ dune runtest 2>&1 | awk '/Internal error/,/Raised/' | sed 's/[0-9]\+/$number/g'
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("unable to serialize exception",
    { exn =
        "Memo.Error.E\n\
        \  { exn =\n\
        \      \"File \\\"dune\\\", line $number, characters $number-$number:\\n\\\n\
        \       Error: No rule found for some_absent_dependency\\n\\\n\
        \       \"\n\
        \  ; stack = [ (\"build-file\", In_build_dir \"default/some_absent_dependency\") ]\n\
        \  }"
    })
  Raised at Stdune__User_error.raise in file
