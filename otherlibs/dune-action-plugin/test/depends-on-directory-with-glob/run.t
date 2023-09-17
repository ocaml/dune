  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
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

  $ cp ./bin/foo.exe ./

  $ dune runtest
  Building some_file!
  Building some_file_but_different!
  some_file
  some_file_but_different
