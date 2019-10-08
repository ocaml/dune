  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (alias
  >  (name runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ mkdir some_dir

  $ cat > some_dir/dune << EOF
  > (rule
  >  (target some_file)
  >  (action
  >   (progn
  >    (echo "Building some_file!\n")
  >    (with-stdout-to %{target} (echo "")))))
  > \
  > (rule
  >  (target another_file)
  >  (action
  >   (progn
  >    (echo "SHOULD NOT BE PRINTED!")
  >    (with-stdout-to %{target} (echo "")))))
  > \
  > (rule
  >  (target some_file_but_different)
  >  (action
  >   (progn
  >    (echo "Building some_file_but_different!\n")
  >    (with-stdout-to %{target} (echo "")))))
  > EOF

  $ cp ../bin/foo.exe ./

  $ dune runtest --display short
           foo alias runtest
  Building some_file!
  Building some_file_but_different!
           foo alias runtest
  some_file
  some_file_but_different
