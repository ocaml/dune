  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target some_file)
  >  (action
  >   (chdir some_dir
  >    (dynamic-run ./foo.exe))))
  > \
  > (rule
  >  (alias runtest)
  >  (action (cat some_file)))
  > EOF

  $ mkdir some_dir

  $ cp ./bin/foo.exe ./some_dir/

  $ dune runtest
  Hello from some_file!
