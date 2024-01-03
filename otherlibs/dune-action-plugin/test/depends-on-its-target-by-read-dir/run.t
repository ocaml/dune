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

  $ dune build some_file 2>&1 | awk '/Internal error/,/unable to serialize/'
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("unable to serialize exception",

^ This is not great. There is no actual dependency cycle, dune is just
interpreting glob dependency too coarsely (it builds all files instead
of just bringing the directory listing up to date).
