  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target some_file)
  >  (action
  >   (dynamic-run ./foo.exe)))
  > EOF

  $ cp ./bin/foo.exe ./

  $ dune build some_file 2>&1 | awk '/Internal error/,/Raised/' | sed 's/[0-9]\+/$number/g'
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("unable to serialize exception",
    { exn =
        "Memo.Error.E\n\
        \  { exn =\n\
        \      \"Cycle_error.E\\n\\\n\
        \      \\  [ (\\\"build-file\\\", In_build_dir \\\"default/some_file\\\")\\n\\\n\
        \      \\  ; (\\\"build-pred\\\",\\n\\\n\
        \      \\    { dir = In_build_dir \\\"default\\\"\\n\\\n\
        \      \\    ; predicate = Glob Glob \\\"**\\\"\\n\\\n\
        \      \\    ; only_generated_files = false\\n\\\n\
        \      \\    })\\n\\\n\
        \      \\  ; (\\\"execute-rule\\\",\\n\\\n\
        \      \\    { id = $number\\n\\\n\
        \      \\    ; info =\\n\\\n\
        \      \\        From_dune_file\\n\\\n\
        \      \\          { pos_fname = \\\"dune\\\"\\n\\\n\
        \      \\          ; start = { pos_lnum = $number; pos_bol = $number; pos_cnum = $number }\\n\\\n\
        \      \\          ; stop = { pos_lnum = $number; pos_bol = $number; pos_cnum = $number }\\n\\\n\
        \      \\          }\\n\\\n\
        \      \\    })\\n\\\n\
        \      \\  ; (\\\"<unnamed>\\\", ())\\n\\\n\
        \      \\  ]\"\n\
        \  ; stack = []\n\
        \  }"
    })
  Raised at Memo.Exec.exec_dep_node.(fun) in file "src/memo/memo.ml", line

^ This is not great. There is no actual dependency cycle, dune is just
interpreting glob dependency too coarsely (it builds all files instead
of just bringing the directory listing up to date).
