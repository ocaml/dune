  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (rule
  >  (target some_file)
  >  (action
  >   (chdir some_dir
  >    (dynamic-run ./foo.exe))))
  > \
  > (alias
  >  (name runtest)
  >  (action (cat some_file)))
  > EOF

  $ mkdir some_dir

  $ cp ../bin/foo.exe ./some_dir/

  $ dune runtest --display short
           foo some_file
  Hello from some_file!
